module Field.Fp6 
  ( Fp6(..)
  , fp6square
  ) where

import Field.Fp 
import Field.Fp2
import qualified Params


data Fp6 = Fp6 Fp2 Fp2 Fp2 
  deriving (Show, Eq)

xi :: Fp2
xi = Fp2 9 1

instance Num Fp6 where
  (+) = fp6add
  (-) = fp6sub
  (*) = fp6mul
  negate = \x -> 0 - x
  fromInteger = new
  abs = undefined
  signum = undefined

instance Fractional Fp6 where
  (/) = \x y -> x * (recip y)
  recip = fp6inv
  fromRational = undefined

new :: Integer -> Fp6
new a = Fp6 (fromInteger a) (fromInteger 0) (fromInteger 0)

fp6add :: Fp6 -> Fp6 -> Fp6
fp6add (Fp6 a0 a1 a2) (Fp6 b0 b1 b2) = (Fp6 c0 c1 c2)
  where
    c0, c1, c2 :: Fp2
    c0 = a0 + b0
    c1 = a1 + b1
    c2 = a2 + b2

fp6sub :: Fp6 -> Fp6 -> Fp6
fp6sub (Fp6 a0 a1 a2) (Fp6 b0 b1 b2) = (Fp6 c0 c1 c2)
  where
    c0, c1, c2 :: Fp2
    c0 = a0 - b0
    c1 = a1 - b1
    c2 = a2 - b2

fp6mul :: Fp6 -> Fp6 -> Fp6
fp6mul (Fp6 a0 a1 a2) (Fp6 b0 b1 b2) = (Fp6 c0 c1 c2)
  where
    c0, c1, c2 :: Fp2
    t0 = a0 * b0
    t1 = a1 * b1
    t2 = a2 * b2
    c0 = ((a1 + a2) * (b1 + b2) - t1 - t2) * xi  + t0
    c1 = (a0 + a1) * (b0 + b1) - t0 - t1 + (xi * t2)
    c2 = (a0 + a2) * (b0 + b2) - t0 - t2 + t1

fp6mulfp2 :: Fp6 -> Fp2 -> Fp6
fp6mulfp2 (Fp6 a0 a1 a2) b0 = Fp6 c0 c1 c2
  where
    c0, c1, c2 :: Fp2
    c0 = a0 * b0
    c1 = a1 * b0
    c2 = a2 * b0

-- multiplication by (Fp6 b0 b1 0)
fp6mulfp2fp2 :: Fp6 -> Fp2 -> Fp2 -> Fp6
fp6mulfp2fp2 (Fp6 a0 a1 a2) b0 b1 = Fp6 c0 c1 c2
  where
    c0, c1, c2 :: Fp2
    t0 = a0 * b0
    t1 = a1 * b1
    c0 = ((a1 + a2) * b1 - t1) * xi + t0;
    c1 = (a0 + a1) * (b0 + b1) - t0 - t1;
    c2 = a2 * b0 + t1


fp6square :: Fp6 -> Fp6
fp6square (Fp6 a0 a1 a2) = Fp6 c0 c1 c2
  where
    c0, c1, c2 :: Fp2
    t1 = a0 * a1
    t2 = t1 + t1
    t3 = a2^2
    c1 = t3 * xi + t2
    c3 = a0^2
    t4 = a1 * a2
    c5 = t4 + t4
    c4 = (a0 - a1 + a2)^2
    c0 = c5 * xi + c3
    c2 = (t2 - t3) + c4 + c5 - c3

fp6inv :: Fp6 -> Fp6
fp6inv (Fp6 a0 a1 a2) = Fp6 c0' c1' c2'
  where 
    t0 = a0 * a0
    t1 = a1 * a1 
    t2 = a2 * a2
    t3 = a0 * a1
    t4 = a0 * a2
    t5 = a1 * a2 
    c0 = t0 - (xi * t5)
    c1 = (xi * t2) - t3
    c2 = t1 - t4 
    t6 = (a0 * c0) + (xi * a2 * c1) + (xi * a1 * c2)
    t6' = recip t6
    c0' = c0 * t6'
    c1' = c1 * t6'
    c2' = c2 * t6'


