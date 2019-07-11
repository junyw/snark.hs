module TestCompiler where

import Test.Tasty
import Test.Tasty.HUnit

import TestUtil

testcases = 
  -- test name, file name, public input, private input
  [ 
    ("test 0", "test0", [], [])
  , ("test 1", "test1", [10], [])
  , ("test 2", "test2", [42], [42])
  
    -- arithmetic expression
  , ("addition 1", "add1", [41], [42])
  , ("addition 2", "add2", [41], [38])
  , ("sum to 100", "add3", [100], [31, 15, 40, 14, 0])

    -- multiplication
  , ("multiplication 1", "mul1", [41], [82])  
  , ("multiplication 2", "mul2", [35], [3])

    -- boolean values
  , ("boolean 1", "bool1", [], [1])
  , ("boolean 2", "bool2", [], [0])
  , ("boolean 3", "bool3", [], [0])

    -- if expression
  , ("if 1", "if1", [], [75])
  , ("if 2", "if2", [1000], [0, 10000])

    -- let expression 
  , ("let 1", "let1", [40], [240])
  ]

compilerTests = testGroup "testing compiler" (mkCompilerTest <$> testcases)

zkTests = testGroup "testing zero knowledge proofs" (mkZKTest <$> testcases)

mkCompilerTest = testCompiler False
mkZKTest       = testCompiler True

