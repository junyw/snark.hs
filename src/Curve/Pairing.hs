module Curve.Pairing
  ( 
    G1, G2, GT
  , g1, g2
  , reducedPairing
  ) where


import Field.Fp
import Field.Fp2
import Field.Fp6
import Field.Fp12
import Field.Fr
import Params (r, p, k, b, t)
import Curve.Curve

import Data.List ((!!), foldl')

-------------------------------------------------------------------------------
-- | G1 
-------------------------------------------------------------------------------

-- | G1 is an additive group of points in elliptic curve E over finite field Fp,
--    with E defined by y^2 = x^3 + b
type G1 = Point Fp

type AffineG1 = Affine Fp

-- | Generator for G1
g1 :: G1
g1 = Point 1 2 1


-------------------------------------------------------------------------------
-- | G2
-------------------------------------------------------------------------------

-- | E adopts a sextic twist E' over finite field Fp2, E' is defined by y^2 = x^3 + b/xi.
--   G2 is an additive group of points in elliptic curve E' over finite field Fp2.
type G2 = Point Fp2

type AffineG2 = Affine Fp2

-- | Generator for G2
g2 :: G2
g2 = Point x y 1
  where
    x = Fp2 
          10857046999023057135944570762232829481370756359578518086990519993285655852781
          11559732032986387107991004021392285783925812861821192530917403151452391805634
    y = Fp2 
          8495653923123431417604973247489272438418190587263600148770280649306958101930
          4082367875863433681332203403145435568316851327593401208105741076214120093531  


xi :: Fp2
xi = Fp2 9 1

-- b/xi
twistCoeffB :: Fp2
twistCoeffB = (fromInteger b)/xi 
            
twoInv :: Fp2
twoInv = (Fp2 1 0)/(Fp2 2 0)

-- | Given a twist E' : y^2 = x^3 + b * c^3,
--   the frobenius is given as (x, y) -> (c^(1 - p) * x^p, c^(3 * (1 - p)/2) * y^p)
frobeniusG2 :: G2 -> G2
frobeniusG2 (Point x y z) = Point x' y' z'
  where 
    coeff1, coeff2 :: Fp2
    coeff1 = xi^((p - 1) `div` 3)
    coeff2 = xi^((p - 1) `div` 2)
    x' = coeff1 * frobenius 1 x
    y' = coeff2 * frobenius 1 y
    z' = frobenius 1 z

frobenius :: Num a =>  Integer -> a -> a
frobenius i x = x^j
  where
    j = Params.p^i


-------------------------------------------------------------------------------
-- | GT
-------------------------------------------------------------------------------

-- | GT is the r-th root of unity in defined in finite field Fp12
type GT = Fp12 

-------------------------------------------------------------------------------
-- | Optimal Ate pairing
-------------------------------------------------------------------------------

-- ell0, ellVW, ellVV
data EllCoeffs
  = EllCoeffs Fp2 Fp2 Fp2
  deriving (Show, Eq)

-- | Optimal Ate pairing 
--   Given P in G1, Q in G2, returns pairing of P and Q
reducedPairing :: G1 -> G2 -> GT
reducedPairing p q 
  | p == pointInf       = error "point P is Point-at-Infinity"
  | q == pointInf       = error "point Q is Point-at-Infinity"
  | otherwise           =  fexp
      where 
        -- Precompute all Q related values
        coeffs :: [EllCoeffs]
        coeffs =  atePrecomputeG2 q
        -- convert P to affine coordinates
        p' = toAffine p
        -- run Miller loop
        f = ateMillerLoop p' coeffs
        -- final exponentiation
        fexp :: GT
        fexp = finalExponentiation f               


-------------------------------------------------------------------------------
-- Miller loop
-------------------------------------------------------------------------------

loopCount :: [Integer]
loopCount = tail $ reverse $ dec2bin (6 * t + 2)
  where 
    dec2bin :: (Integral a) => a -> [a]
    dec2bin 0 = []
    dec2bin x = (x `mod` 2) : dec2bin (x `quot` 2)


ateMillerLoop :: AffineG1 -> [EllCoeffs] -> GT
ateMillerLoop p coeffs  = finalF
  where
    -- Main loop
    postLoopF :: Fp12
    (postLoopIx, postLoopF) = foldl' (ateLoopBody p coeffs) (0, 1) loopCount
    
    -- f <- f * lR,Q1(P), R <- R + Q1
    almostF = mulBy024 postLoopF (prepareCoeffs coeffs p postLoopIx)

    -- f <- f * lR,-Q2(P), R <- R - Q1
    finalF  = mulBy024 almostF (prepareCoeffs coeffs p (postLoopIx + 1))


ateLoopBody :: AffineG1 -> [EllCoeffs] -> (Int, Fp12) -> Integer -> (Int, Fp12)
ateLoopBody p coeffs (idx, f) currentBit =
  let 
    f' = mulBy024 (f^2) (prepareCoeffs coeffs p idx)
  in
    if currentBit == 1
        then (idx + 2, mulBy024 f' (prepareCoeffs coeffs p (idx + 1)))
        else (idx + 1, f')

prepareCoeffs :: [EllCoeffs] -> AffineG1 -> Int -> EllCoeffs
prepareCoeffs _ Infinity _ = error "prepareCoeffs: received trivial point"
prepareCoeffs coeffs (Affine px py) ix = EllCoeffs ell0 ellVWpy ellVVpx
  where 
    (EllCoeffs ell0 ellVW ellVV) = coeffs !! ix
    ellVWpy = fp2mulfp ellVW py
    ellVVpx = fp2mulfp ellVV px

mulBy024 :: Fp12 -> EllCoeffs -> Fp12
mulBy024 this (EllCoeffs ell0 ellVW ellVV) = this * a
  where 
    a = Fp12 (Fp6 ell0 0 ellVV) (Fp6 0 ellVW 0)

-------------------------------------------------------------------------------
-- Precomputation on G2
-------------------------------------------------------------------------------

atePrecomputeG2 :: G2 -> [EllCoeffs]
atePrecomputeG2 origPt = reverse finalCoeffs
  where
    bigQ = toJacobian (toAffine origPt) -- make Zq = 1

    -- precompute Q for main loop
    (postLoopR, postLoopCoeffs) = runLoop bigQ
    bigQ1 = frobeniusG2 bigQ                      -- Q1 <- frobenius(Q)
    bigQ2 = pointNeg $ frobeniusG2 bigQ1          -- Q2 <- frobenius^2(Q), -Q2

    -- precompute: f <- f * lR,Q1(P), R <- R + Q1 
    (newR, coeffs1) = mixedAdditionStepForFlippedMillerLoop bigQ1 postLoopR
    
    -- precompute:f <- f * lR,-Q2(P), R <- R - Q1
    (_, coeffs2) = mixedAdditionStepForFlippedMillerLoop bigQ2 newR
    finalCoeffs =  [coeffs1, coeffs2]++ postLoopCoeffs
      
    -- Assumes Q to have Z coordinate to be 1
    runLoop q = foldl' (loopBody q) (q, []) loopCount

    loopBody :: G2 -> (G2, [EllCoeffs]) -> Integer -> (G2, [EllCoeffs])
    loopBody q (r, coeffs) currentBit =
        let (r', coeff) = doublingStepForFlippedMillerLoop r in
        let coeffs' = coeff:coeffs in
        if currentBit == 1
          then
            let (r'', coeff') = mixedAdditionStepForFlippedMillerLoop q r'
              in (r'', coeff':coeffs')
          else (r', coeffs')


doublingStepForFlippedMillerLoop :: G2 -> (G2, EllCoeffs)
doublingStepForFlippedMillerLoop (Point x1 y1 z1) = ((Point x3 y3 z3), EllCoeffs ell0 ellVW ellVV)
  where
    a, b, c, d, e, f, g, h, i, j :: Fp2
    a = (x1 * y1) * twoInv                                  -- A = X1 * Y1/2
    b = y1^2                                                -- B = Y1^2
    c = z1^2                                                -- C = Z1^2
    d = 3 * c                                               -- D = 3 * C
    e = twistCoeffB * d                                     -- E = twist_b * D
    f = 3 * e                                               -- F = 3 * E
    g =  (b + f) * twoInv                                   -- G = (B + F)/2
    h = (y1 + z1) * (y1 + z1) - (b + c)                     -- H = (Y1 + Z1)^2 - (B + C)
    i = e - b                                               -- I = E - B
    j = x1^2                                                -- J = X1^2

    x3 = a * (b - f)                                        -- X3 = A * (B - F)
    y3 = g * g - (3 * e^2)                                  -- Y3 = G^2 - 3 * E^2
    z3 = b * h                                              -- Z3 = B * H

    ell0, ellVW, ellVV :: Fp2 
    ell0  = xi * i                                          -- ell0 = xi * I                
    ellVW = - h                                             -- ellVW = -H, to be multiplied by yp
    ellVV = 3 * j                                           -- ellVV = 3 * J, to be multiplied by xp


mixedAdditionStepForFlippedMillerLoop :: G2 -> G2 -> (G2, EllCoeffs)
mixedAdditionStepForFlippedMillerLoop _base@(Point x2 y2 _z2) _current@(Point x1 y1 z1) = 
    ((Point x3 y3 z3), EllCoeffs ell0 ellVW ellVV)
  where 
    d, e, f, g, h, i, j :: Fp2
    d = x1 - (x2 * z1)                                      -- D = X1 - X2 * Z1
    e = y1 - (y2 * z1)                                      -- E = Y1 - Y2 * Z1
    f = d^2                                                 -- F = D^2
    g = e^2                                                 -- G = E^2
    h = d * f                                               -- H = D * F
    i = x1 * f                                              -- I = X1 * F
    j = h + z1 * g - 2 * i                                  -- J = H + Z1 * G - 2 * I

    x3 = d * j                                              -- X3 = D * J
    y3 = e * (i - j) - (h * y1)                             -- Y3 = E * (I - J) - H * Y1
    z3 = z1 * h                                             -- Z3 = Z1 * H

    ell0, ellVW, ellVV :: Fp2        
    ell0  = xi * (e * x2 - d * y2)                          -- ell0 = xi * (E + X2 - D * Y2)
    ellVW = d                                               -- ellVW = D, to be multiplied by yp
    ellVV = - e                                             -- ellVV = - E, to be multiplied by xp                

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | final exponentiation: 
--   the exponent e is split into two parts: 
--  e = (p^12 - 1)/r = (p^6 - 1) * (p^2 + 1) * (p^4 - p^2 + 1)/r

finalExponentiation :: Fp12 -> GT
finalExponentiation f = (finalExponentiationFirstChunk f) ^ expVal
  where
    expVal = div (p^4 - p^2 + 1) r

finalExponentiationFirstChunk f | f == 0   = 0
finalExponentiationFirstChunk f@(Fp12 a b) = frobenius 2 newf0 * newf0 
  where
    f1 = Fp12 a (-b)
    newf0 = f1/f              

