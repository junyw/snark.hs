module Curve.Curve
  (   
  -- * Group Computations
    Point(..), Affine(..), toJacobian, toAffine
  , pointAdd
  , pointDbl, pointMul, pointNeg, pointInf

  -- * Helper Functions
  , scalarFr
  , innerG1
  , innerG2
  ) where

import Field.Fp
import Field.Fp2
import Field.Fp6
import Field.Fp12
import Field.Fr
import qualified Params

-------------------------------------------------------------------------------
-- | Affine Coordinates
-------------------------------------------------------------------------------

data Affine a
  = Affine a a    --  Affine point
  | Infinity      --  Point at infinity
  deriving (Eq, Show)


toJacobian :: (Eq k, Fractional k) => Affine k -> Point k
toJacobian Infinity     = Point 1 1 0
toJacobian (Affine x y) = Point x y 1

-------------------------------------------------------------------------------
-- | Group Computations in Jacobian Coordinates
-------------------------------------------------------------------------------

-- | Jacobian coordinates for points on an elliptic curve over a finite field 
data Point a = Point a a a
  deriving (Show)

toAffine :: (Eq a, Fractional a) => (Point a) -> (Affine a)
toAffine (Point x y z) = Affine (x/z^2) (y/z^3)

instance (Eq a, Fractional a) => Eq (Point a) where
  (Point x1 y1 z1) == (Point x2 y2 z2) = x1 * z2^2 == x2 * z1^2 && y1 * z2^3 == y2 * z1 ^3 

-- point at infinity
pointInf :: (Eq a, Fractional a) => Point a
pointInf = Point (fromInteger 1) (fromInteger 1) (fromInteger 0) 

-- Explicit-Formulas Database
-- "dbl-2009-l"
pointDbl :: (Eq a, Fractional a) => (Point a) -> (Point a)
pointDbl (Point _ _ 0) = pointInf
pointDbl (Point x1 y1 z1) = (Point x3 y3 z3)
  where 
    a = x1^2
    b = y1^2
    c = b^2
    d = 2 * ((x1 + b)^2 - a - c)
    e = 3 * a
    f = e^2
    x3 = f - 2 * d
    y3 = e * (d - x3) - 8 * c
    z3 = 2 * y1 * z1

-- Explicit-Formulas Database
-- "add-2007-bl" 
pointAdd :: (Eq a, Fractional a) => (Point a) -> (Point a) -> (Point a)
pointAdd p q | p == pointInf = q
pointAdd p q | q == pointInf = p
pointAdd p q | p == q        = pointDbl p
pointAdd (Point x1 y1 z1) (Point x2 y2 z2) = (Point x3 y3 z3)
  where
    z1z1 = z1^2
    z2z2 = z2^2
    u1 = x1 * z2z2
    u2 = x2 * z1z1
    s1 = y1 * z2 * z2z2 
    s2 = y2 * z1 * z1z1 
    h = u2 - u1
    i = (2 * h)^2
    j = h * i
    r = 2 * (s2 - s1) 
    v = u1 * i
    x3 = r^2 - j - 2*v
    y3 = r*(v - x3) - 2 * s1 * j
    z3 = ((z1 + z2)^2 - z1z1 - z2z2) * h

-- multiplication-by-m map
pointMul :: (Eq a, Fractional a) => (Point a) -> Integer -> (Point a)
pointMul p 0 = pointInf 
pointMul p 1 = p
pointMul p n 
  | n < 0    = error "pointMul: negative parameter"
  | even n   =  (pointDbl p) `pointMul` (n `div` 2)
  | otherwise = p `pointAdd` ((pointDbl p) `pointMul` (n `div` 2))


pointNeg :: (Eq a, Fractional a) => (Point a) -> (Point a) 
pointNeg (Point x y z) = Point x (-y) z


-------------------------------------------------------------------------------
-- | Helper functions
-------------------------------------------------------------------------------

scalarFr :: (Eq a, Fractional a) => (Point a) -> Fr -> (Point a)
scalarFr g m = g `pointMul` (asInteger m)


innerG1 :: (Eq a, Fractional a) => [(Point a)] -> [Fr] -> (Point a)
innerG1 gs bs 
  | length gs == length bs = foldl (\acc v -> acc `pointAdd` v) pointInf (zipWith scalarFr gs bs)
  | otherwise = error $ "Vector sizes must match " ++ (show $ length gs) ++ " - " ++ (show $ length bs)

innerG2 :: (Eq a, Fractional a) => [(Point a)] -> [Fr] -> (Point a)
innerG2 gs bs 
  | length gs == length bs = foldl (\acc v -> acc `pointAdd` v) pointInf (zipWith scalarFr gs bs)
  | otherwise = error "Vector sizes must match"

