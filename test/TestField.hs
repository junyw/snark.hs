module TestField where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Field.Fp
import Field.Fp2
import Field.Fp6
import Field.Fp12
import Field.Fr
import TestUtil

import Data.Proxy

-------------------------------------------------------------------------------
-- | Field Laws
-------------------------------------------------------------------------------

testFieldLaws
  :: forall a. (Num a, Fractional a, Eq a, Arbitrary a, Show a)
  => Proxy a
  -> TestName
  -> TestTree
testFieldLaws _ name
  = testGroup ("Test field laws of " ++ name)
    [ testProperty "commutativity of addition"
      $ commutes ((+) :: a -> a -> a)
    , testProperty "commutativity of multiplication"
      $ commutes ((*) :: a -> a -> a)
    , testProperty "associativity of addition"
      $ associates ((+) :: a -> a -> a)
    , testProperty "associativity of multiplication"
      $ associates ((*) :: a -> a -> a)
    , testProperty "additive identity"
      $ isIdentity ((+) :: a -> a -> a) 0
    , testProperty "multiplicative identity"
      $ isIdentity ((*) :: a -> a -> a) 1
    , testProperty "additive inverse"
      $ isInverse ((+) :: a -> a -> a) negate 0
    , testProperty "multiplicative inverse"
      $ \x -> (x /= (0 :: a)) ==> isInverse ((*) :: a -> a -> a) recip 1 x
    , testProperty "multiplication distributes over addition"
      $ distributes ((*) :: a -> a -> a) (+)
    ]
--------------------------------------------------------------------------------
-- | Generators
--------------------------------------------------------------------------------

instance Arbitrary Fp where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Fp2 where
  arbitrary = Fp2 <$> arbitrary <*> arbitrary

instance Arbitrary Fp6 where
  arbitrary = Fp6 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Fp12 where
  arbitrary = Fp12 <$> arbitrary <*> arbitrary

instance Arbitrary Fr where
  arbitrary = fromInteger <$> arbitrary 

--------------------------------------------------------------------------------
-- | Tests 
--------------------------------------------------------------------------------

test_Fp :: TestTree
test_Fp = testFieldLaws (Proxy :: Proxy Fp) "Fp"

test_Fp2 :: TestTree
test_Fp2 = testFieldLaws (Proxy :: Proxy Fp2) "Fp2"

test_Fp6 :: TestTree
test_Fp6 = testFieldLaws (Proxy :: Proxy Fp6) "Fp6"

test_Fp12 :: TestTree
test_Fp12 = testFieldLaws (Proxy :: Proxy Fp12) "Fp12"

test_Fr :: TestTree
test_Fr = testFieldLaws (Proxy :: Proxy Fr) "Fr"

tests :: TestTree
tests = testGroup "Test finite field computations" [test_Fp, test_Fp2, test_Fp6, test_Fp12, test_Fr]

