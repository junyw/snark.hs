module Compiler.QAP
  ( 
    QAP(..)
  , r1csToQAP
  , qapSat
  ) where

import Data.Map.Strict as Map

import Field.Fr
import Compiler.R1CS
import Util.Polynomial

--------------------------------------------------------------------------------
-- | Quadratic Arithmetic Program
--------------------------------------------------------------------------------

{-
Given a program with 
  numberOfPublicInputs  = n
  numberOfPrivateInputs = h
  numberOfOutputs       = l
  numberOfWires         = a
  numberOfGates         = b

'r1csToQAP' converts the program to a QAP with
  size of the QAP     m = a
  degree of Z         d = n + b + l + 1
  qapA, qapB, qapC     :  groups of m + 1 polynomials, each with degree <= d - 1
  qapA                 :  [(&1, A0), (&var_1, A1), ...,(&var_n, An), (var_n+1, An+1),..., (var_m ,Am)]
  qapB                 :  [(&1, B0), (&var_1, B1), ...,(&var_n, Bn), (var_n+1, Bn+1),..., (var_m ,Bm)]
  qapC                 :  [(&1, C0), (&var_1, C1), ...,(&var_n, Cn), (var_n+1, Cn+1),..., (var_m ,Cm)]
    where &1 is a special input, &var_1 to &var_n are n public inputs, var_n+1 to var_m are m - n verifier inputs
  qapZ                 : a polynomial of degree d
-}

data QAP a = QAP { sizeOfPublicInputs :: Integer
                 , degreeOfZ          :: Integer
                 , qapA :: !(Map String (Poly a)) -- size m + 1
                 , qapB :: !(Map String (Poly a)) -- size m + 1
                 , qapC :: !(Map String (Poly a))-- size m + 1
                 , qapZ :: !(Poly a)
                 } 
  deriving (Show)


r1csToQAP :: R1CS -> QAP Fr
r1csToQAP  
  R1CS{ r1csA, r1csB, r1csC, degree, r1csN } = 
  QAP { sizeOfPublicInputs = r1csN
      , degreeOfZ = degree
      , qapA = interp degree r1csA
      , qapB = interp degree r1csB 
      , qapC = interp degree r1csC
      , qapZ = target degree
      }
  where 
    interp size env = Map.map (interpolation size) env

--------------------------------------------------------------------------------
-- | QAP Satisfaction 
--------------------------------------------------------------------------------

{- 
INPUTS: 
  a quadratic arithmetic program 
  an assignment of all inputs (size = m), consists of public inputs &var_1 to &var_n and verifier inputs var_n+1 to var_m
    The special input &1 is assigned to 1 and added to the front, giving a total size of m+1. 
    
OUTPUT: True if the given assignment satisfies the QAP
-}

type Env a  = Map.Map String a

qapSat :: QAP Fr -> Env Fr -> Bool
qapSat QAP{ degreeOfZ, qapA, qapB, qapC, qapZ } values = 
  let 
    vals = 1:Map.elems values

    a_poly = innerPolyFr (Map.elems qapA) vals 
    b_poly = innerPolyFr (Map.elems qapB) vals 
    c_poly = innerPolyFr (Map.elems qapC) vals 

    (_, re) = ((a_poly * b_poly) - c_poly) `divmod` qapZ
  in 
    (deg re) == 0


