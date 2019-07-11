module Field.Fp12 
  ( Fp12(..)
  , fromInteger
  , fp12square
  ) where

import Field.Fp 
import Field.Fp2
import Field.Fp6
import qualified Params

gamma :: Fp6
gamma = Fp6 0 1 0

data Fp12 = Fp12 Fp6 Fp6
  deriving (Show, Eq)

instance Num Fp12 where
  (+) = fp12add
  (-) = fp12sub
  (*) = fp12mul
  negate = \x -> 0 - x
  fromInteger = new
  abs = undefined
  signum = undefined

instance Fractional Fp12 where
  (/) = \x y -> x * (recip y)
  recip = fp12inv
  fromRational = undefined

new :: Integer -> Fp12
new a = Fp12 (fromInteger a) (fromInteger 0)

fp12add :: Fp12 -> Fp12 -> Fp12
fp12add (Fp12 a0 a1) (Fp12 b0 b1) = Fp12 c0 c1
  where
    c0, c1 :: Fp6
    c0 = a0 + b0
    c1 = a1 + b1


fp12sub :: Fp12 -> Fp12 -> Fp12
fp12sub (Fp12 a0 a1) (Fp12 b0 b1) = Fp12 c0 c1
  where
    c0, c1 :: Fp6
    c0 = a0 - b0
    c1 = a1 - b1

fp12mul :: Fp12 -> Fp12 -> Fp12
fp12mul (Fp12 a0 a1) (Fp12 b0 b1) = Fp12 c0 c1
  where
    c0, c1 :: Fp6
    t0 = a0 * b0
    t1 = a1 * b1
    c0 = t0 + (t1 * gamma)
    c1 = (a0 + a1) * (b0 + b1) - t0 - t1

fp12square :: Fp12 -> Fp12
fp12square (Fp12 a0 a1) = Fp12 c0 c1
  where
    c0, c1 :: Fp6
    t1 = a0 - (gamma * a1)
    t2 = a0 * a1
    t3 = (a0 - a1) * t1 + t2
    c1 = t2 + t2
    c2 = gamma * t2
    c0 = t3 + c2

fp12inv :: Fp12 -> Fp12
fp12inv (Fp12 a0 a1) = Fp12 c0 c1
  where
    c0, c1 :: Fp6
    t1 = fp6square a1
    t0 = (fp6square a0) - (gamma * t1)
    t2 = recip t0
    c0 = a0 * t2
    c1 = negate (a1 * t2)







