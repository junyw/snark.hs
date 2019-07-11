module Main where

import Test.Tasty
import TestField
import TestCurve
import TestCompiler

main :: IO ()
main = defaultMain $ testGroup "all tests" 
  [ TestField.tests
  , TestCurve.tests
  , TestCompiler.compilerTests
  , TestCompiler.zkTests
  ]
