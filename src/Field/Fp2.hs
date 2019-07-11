module Field.Fp2 
  ( Fp2(..)
  , fromInteger
  , fp2mulfp
  ) where

import Field.Fp 
import qualified Params


data Fp2 = Fp2 Fp Fp
  deriving (Show, Eq)

beta :: Fp
beta = fromInteger Params.beta

instance Num Fp2 where
  (+) = fp2add
  (-) = fp2sub
  (*) = fp2mul
  negate = \x -> 0 - x
  fromInteger = new
  abs = undefined
  signum = undefined

instance Fractional Fp2 where
  (/) = \x y -> x * (recip y)
  recip = fp2inv
  fromRational = undefined

new :: Integer -> Fp2
new a = Fp2 (fromInteger a) (fromInteger 0)

fp2add :: Fp2 -> Fp2 -> Fp2
fp2add (Fp2 a0 a1) (Fp2 b0 b1) = Fp2 (a0 + b0) (a1 + b1)

fp2sub :: Fp2 -> Fp2 -> Fp2
fp2sub (Fp2 a0 a1) (Fp2 b0 b1) = Fp2 (a0 - b0) (a1 - b1)

fp2mul :: Fp2 -> Fp2 -> Fp2
fp2mul (Fp2 a0 a1) (Fp2 b0 b1) = Fp2 c0 c1
  where
    c0, c1 :: Fp
    t0 = a0 * b0
    t1 = a1 * b1
    c0 = t1 * beta + t0
    c1 = (a0 + a1) * (b0 + b1) - t0 - t1

fp2mulfp :: Fp2 -> Fp -> Fp2
fp2mulfp (Fp2 a0 a1) b0 = Fp2 c0 c1 
  where 
    c0, c1 :: Fp
    c0 = a0 * b0
    c1 = a1 * b0

fp2inv :: Fp2 -> Fp2
fp2inv (Fp2 a0 a1) = (Fp2 c0 c1) 
  where
    c0, c1 :: Fp
    t0 = a0 * a0
    t1 = a1 * a1
    t0' = t0 - (beta * t1)
    t1' = recip t0'
    c0 = a0 * t1' 
    c1 = negate a1 * t1'

