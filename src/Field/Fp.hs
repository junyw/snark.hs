module Field.Fp 
  ( Fp(..)
  , AsInteger(..)
  , fromInteger
  ) where

import qualified Params

newtype Fp = Fp Integer 
  deriving (Show, Eq)

instance Num Fp where
  (+) = fpadd
  (-) = fpsub
  (*) = fpmul
  negate = fpneg
  fromInteger = new
  abs = undefined
  signum = undefined

instance Fractional Fp where
  (/) = fpdiv
  recip = fpinv
  fromRational = undefined

class AsInteger a where
  asInteger :: a -> Integer

instance AsInteger Fp where
  asInteger (Fp n) = n

new :: Integer -> Fp
new a = Fp $ a `mod` Params.p


{-# INLINE fpadd #-}
fpadd :: Fp -> Fp -> Fp
fpadd (Fp a) (Fp b) = Fp $ ((a Prelude.+ b) `mod` Params.p)

{-# INLINE fpsub #-}
fpsub :: Fp -> Fp -> Fp
fpsub (Fp a) (Fp b) = Fp $ ((a Prelude.- b) `mod` Params.p)

{-# INLINE fpneg #-}
fpneg :: Fp -> Fp
fpneg (Fp a) = Fp $ (Prelude.negate a) `mod` Params.p

{-# INLINE fpmul #-}
fpmul :: Fp -> Fp -> Fp
fpmul (Fp a) (Fp b) = Fp $ ((a Prelude.* b) `mod` Params.p)

{-# INLINE fpdiv #-}
fpdiv :: Fp -> Fp -> Fp
fpdiv a b = a * (fpinv b) 

{-# INLINE fpinv #-}
fpinv :: Fp -> Fp
fpinv (Fp 0) = error "divde-by-zero"
fpinv (Fp a) = Fp $ x
  where 
    (x, y) = gcdExt a Params.p

gcdExt :: Integer -> Integer -> (Integer, Integer)
gcdExt a 0 = (1, 0)
gcdExt a b = (t, s Prelude.- q Prelude.* t)
  where (q, r) = quotRem a b
        (s, t) = gcdExt b r

