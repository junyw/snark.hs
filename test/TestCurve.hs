module TestCurve where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Params
import Field.Fp
import Field.Fp2
import Curve.Curve
import Curve.Pairing
import TestUtil

-------------------------------------------------------------------------------
-- | Abelian Group Laws
-------------------------------------------------------------------------------

testGroupLaws
  :: (Eq a, Arbitrary a, Show a)
  => (a -> a -> a)
  -> (a -> a)
  -> a
  -> TestName
  -> TestTree
testGroupLaws add neg ident name
  = testGroup ("Test Abelian group laws of " ++ name)
    [ testProperty "commutativity of addition"
      $ commutes   add
    , testProperty "associavity of addition"
      $ associates add 
    , testProperty "additive identity"
      $ isIdentity add ident
    , testProperty "additive inverse"
      $ isInverse  add neg ident
    ]

--------------------------------------------------------------------------------
-- | Generators
--------------------------------------------------------------------------------

instance Arbitrary G1 where
  arbitrary = pointMul g1 . abs <$> (choose (1, 100000))

instance Arbitrary G2 where 
  arbitrary = pointMul g2 . abs <$> (choose (1, 100000))

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [test_G1, test_G1Mul, test_G2, test_G2Mul, test_pairing]

-------------------------------------------------------------------------------
-- | G1 Tests
-------------------------------------------------------------------------------

test_G1 :: TestTree
test_G1
  = testGroupLaws pointAdd pointNeg (pointInf:: G1) "G1"

test_G1Mul :: TestTree
test_G1Mul = testProperty "Scalar multiplication of G1"
      $ \p (Positive a) (Positive b) -> (p :: G1) `pointMul` (a :: Integer) `pointMul` (b :: Integer) ==  p `pointMul` (a * b)

-------------------------------------------------------------------------------
-- | G2 Tests
-------------------------------------------------------------------------------

test_G2 :: TestTree
test_G2
  = testGroupLaws pointAdd pointNeg (pointInf:: G2) "G2"

test_G2Mul :: TestTree
test_G2Mul = testProperty "Scalar multiplication of G2"
      $ \p (Positive a) (Positive b) -> (p :: G2) `pointMul` (a :: Integer) `pointMul` (b :: Integer) ==  p `pointMul` (a * b)


-------------------------------------------------------------------------------
-- | Pairing Tests
-------------------------------------------------------------------------------
pairing = reducedPairing

test_pairing :: TestTree
test_pairing = testProperty "Test Optimal Ate pairing" $ withMaxSuccess 10 
      $ \p q (Positive a) (Positive b) -> 
            let pair1 = pairing (p :: G1) (q :: G2) in
            let c = (a :: Integer) * (b :: Integer) in
            let pair2 = pairing (p `pointMul` a) (q `pointMul` b) in
              pair1^c == pair2

