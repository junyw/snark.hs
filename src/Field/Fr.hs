module Field.Fr 
  ( Fr(..)
  , fromInteger
  , asInteger
  , random
  ) where

import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Field.Fp (AsInteger(..))

import qualified Params

newtype Fr = Fr Integer 
  deriving (Show, Eq)

instance Num Fr where
  (+) = fradd
  (-) = frsub
  (*) = frmul
  negate = frneg
  fromInteger = new
  abs = undefined
  signum = undefined

instance Fractional Fr where
  (/) = frdiv
  recip = frinv
  fromRational = undefined

instance AsInteger Fr where
  asInteger (Fr n) = n

new :: Integer -> Fr
new a = Fr $ a `mod` Params.r

{-# INLINE fradd #-}
fradd :: Fr -> Fr -> Fr
fradd (Fr a) (Fr b) = Fr $ ((a Prelude.+ b) `mod` Params.r)

{-# INLINE frsub #-}
frsub :: Fr -> Fr -> Fr
frsub (Fr a) (Fr b) = Fr $ ((a Prelude.- b) `mod` Params.r)

{-# INLINE frneg #-}
frneg :: Fr -> Fr
frneg (Fr a) = Fr $ (Prelude.negate a) `mod` Params.r

{-# INLINE frmul #-}
frmul :: Fr -> Fr -> Fr
frmul (Fr a) (Fr b) = Fr $ ((a Prelude.* b) `mod` Params.r)

{-# INLINE frdiv #-}
frdiv :: Fr -> Fr -> Fr
frdiv a b = a * (frinv b) 

{-# INLINE frinv #-}
frinv :: Fr -> Fr
frinv (Fr 0) = error "divde-by-zero"
frinv (Fr a) = Fr $ x
  where 
    (x, y) = gcdExt a Params.r

gcdExt :: Integer -> Integer -> (Integer, Integer)
gcdExt a 0 = (1, 0)
gcdExt a b = (t, s Prelude.- q Prelude.* t)
  where (q, r) = quotRem a b
        (s, t) = gcdExt b r

-- random element
random :: MonadRandom m => m Fr
random = do
  v <- generateMax Params.r
  pure (Fr v)

