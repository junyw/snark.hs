module Util.Polynomial 
  ( Poly(..)
  , divmod
  , eval
  , norm
  , scale
  , interpolation
  , deg
  , toList
  , target
  , innerPolyFr
  ) where

import Data.List

import Field.Fp
import Field.Fr

-- a polynomial of degree d "c0 + c1x + ... + cdx^d" is 
-- represented as a list [cd, ..., c1, c0]
newtype Poly a = P [a] 
  deriving (Show)

  
instance (Eq a, Num a) => Num (Poly a) where
  (P a) + (P b) = P $ polyAdd a b
  (P a) * (P b) = P $ polyMul a b
  (P a) - (P b) = P $ polySub a b  
  negate (P a) = P $ polyNeg a
  fromInteger = \n -> P $ (fromInteger n):[]
  abs = undefined
  signum = undefined

instance (Eq a, Num a) => Eq (Poly a) where
  (P a) == (P b) = (norm' a) == (norm' b)

toList :: (Poly a) -> [a]
toList (P a) = a

eval :: (Eq a, Num a) => (Poly a) -> a -> a
eval (P coeffs) x = foldl (\a b -> b + a*x) 0 coeffs

scale :: (Eq a, Num a) => (Poly a) -> a -> (Poly a)
scale (P a) n = P $ scale' a n

{-# INLINE scale' #-}
scale' :: (Eq a, Num a) => [a] -> a -> [a]
scale' p a = map (a*) p

norm :: (Eq a, Num a) => (Poly a) -> (Poly a)
norm (P a) = P $ norm' a

norm' :: (Eq a, Num a) => [a] -> [a]
norm' = dropWhile (== 0)

deg :: (Eq a, Num a) => (Poly a)  -> Int
deg (P a) = deg' a

deg' :: (Eq a, Num a) => [a] -> Int
deg' ls = max 0 (length (norm' ls))


-------------------------------------------------------------------------------
-- Polynomial ring computations
-------------------------------------------------------------------------------

{-# INLINE polyAdd #-}
polyAdd :: (Eq a, Num a) => [a] -> [a] -> [a]
polyAdd = zip' (+)

{-# INLINE polySub #-}
polySub :: (Eq a, Num a) => [a] -> [a] -> [a]
polySub = zip' (-)

{-# INLINE polyMul #-}
polyMul :: (Eq a, Num a) => [a] -> [a] -> [a]
polyMul f g | length g >= length f = let (p, _) = foldr (\a (p, n) -> (p `polyAdd` (shift n (scale' g a)), n+1)) ([], 0) f in p
polyMul f g = polyMul g f 

polyNeg :: (Eq a, Num a) => [a] -> [a]
polyNeg = map negate

divmod :: (Eq a, Num a, Fractional a) => (Poly a) -> (Poly a) -> ((Poly a), (Poly a))
divmod (P a) (P b) = (P q, P r) 
  where 
    (q, r) = divmod' a b

-- https://rosettacode.org/wiki/Polynomial_long_division
divmod' :: (Eq a, Num a, Fractional a) => [a] -> [a] -> ([a], [a])
divmod' f g = aux (norm' f) (norm' g) []
  where aux f s q | ddif < 0 = (q, f)
                  | otherwise = aux f' s q'
           where ddif = (deg' f) - (deg' s)
                 k = (head f) / (head s)
                 ks = (shift ddif s) `scale'` k
                 q' = q `polyAdd` (shift ddif [k])
                 f' = norm' $ polySub f ks

shift :: (Eq a, Num a) => Int -> [a] -> [a]
shift n ls = if n <= 0 then ls else ls ++ replicate n (fromInteger 0)

pad :: (Eq a, Num a) => Int -> [a] -> [a]
pad n ls = if n <= 0 then ls else replicate n (fromInteger 0) ++ ls

zip' :: (Eq a, Num a) => (a -> a -> a) -> [a] -> [a] -> [a]
zip' op p q = zipWith op (pad (-dif) p) (pad dif q)
  where 
    dif = length p - length q

--------------------------------------------------------------------------------
-- | Lagrange Interpolation
--------------------------------------------------------------------------------

-- size: total number of points, 
-- x: the x-coordinate of the target point, in [1,...,size]
-- y: the y-coordinate of the target point
-- returns a new polynomial l(x) such that l(x) = y at the target point and l(x) = 0 at all the other given points
basis :: (Eq a, Num a, Fractional a) => Integer -> a -> a -> [a]
basis size x y = 
  let (fac, prod) = 
        foldl 
        (\(fac, prod) xi -> if xi /= x 
            then (fac * (x - xi), prod `polyMul` [1, (-xi)]) 
            else (fac, prod)
        ) (1, [convert 1]) (map convert [1..size])
  in
    prod `scale'` (y/fac)
  where 
    convert = fromInteger . toInteger


interpolation :: (Eq a, Num a, Fractional a) => Integer -> [(Integer, Integer)] -> (Poly a)
interpolation size coords = P $ interpolation' size coords

-- given a list of points [(x1, y1),..., (xn, yn)], return a polynomial L(x) such that
-- L(x1) = y1, L(x2) = y2, ..., L(xn) = yn
-- and L(x) = 0 for all other x in [1..size]
interpolation' :: (Eq a, Num a, Fractional a) => Integer -> [(Integer, Integer)] -> [a]
interpolation' size coords = 
  foldl (\ acc (xi, yi) -> acc `polyAdd` (basis size xi yi)) [] (map (\(x, y) -> (convert x, convert y)) coords)
  where 
    convert = fromInteger . toInteger


-------------------------------------------------------------------------------
-- | Helper functions
-------------------------------------------------------------------------------

target :: (Eq a, Num a, Fractional a) => Integer -> Poly a
target d = P $ foldl (\p v -> p `polyMul` [fromInteger 1, fromInteger $ toInteger (-v)]) [fromInteger 1] [1..d]


innerPolyFr :: [Poly Fr] -> [Fr] -> Poly Fr
innerPolyFr as bs | length as == length bs = sum (zipWith scale as bs)
                  | otherwise = error $ "Vector sizes must match " ++ (show $ length as) ++ " " ++ (show $ length bs)

-------------------------------------------------------------------------------
-- | Printing
-------------------------------------------------------------------------------

strpoly l = intercalate " + " $ terms l
  where term v 0 = show v
        term 1 1 = "x"
        term v 1 = (show v) ++ "x"
        term 1 p = "x^" ++ (show p)
        term v p = (show v) ++ "x^" ++ (show p)
 
        terms :: (Show a, Eq a, Num a) => [a] -> [String]
        terms [] = []
        terms (0:t) = terms t
        terms (h:t) = (term h (length t)) : (terms t)


