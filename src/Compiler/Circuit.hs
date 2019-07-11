module Compiler.Circuit 
  ( 
    Circuit, ICircuit(..)
  , CPrim2(..)
  , CGate, IGate(..)
  , CWire, IWire(..)
  , toName
  , negateWires
  , negateWire
  , circuitToR1CS
  , qapWitness
  , Pretty(..)
  , setCircuitValue
  ) where

import Field.Fr
import Compiler.R1CS as R1CS
import Compiler.QAP
import Compiler.AST (Prim2)

import Control.Exception
import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc

--------------------------------------------------------------------------------
-- | Circuit
--------------------------------------------------------------------------------

data ICircuit a = Circuit { cPublicParams  :: ![IWire a] 
                          , cPrivateParams :: ![IWire a] 
                          , cGates         :: ![IGate a]
                          , cWires         :: ![IWire a]
                          }
  deriving (Show, Functor)

-- | Operation, left wires, right wires, output wire
data IGate a = CGate      CPrim2    [IWire a] [IWire a] (IWire a)  -- gate that represents some computation
             | CAssert    CPrim2    [IWire a] [IWire a] (IWire a)  -- gate that represents some constraint, where the output is a fixed constant
             | CNormalize [IWire a] (IWire a) (IWire a)            -- gate with an auxiliary wire that normalizes input to one
  
  deriving (Show, Functor)

-- | CPrim2 are bianry gate operations
data CPrim2 = CTimes | CDivide  
  deriving (Show)

data IWire a = CWire a Integer 
             | CInt Integer
  deriving (Show, Functor)

type Id      = String 
type Circuit = ICircuit Id
type CWire   = IWire Id
type CGate   = IGate Id 

---------------------------------------------------------------------------

toName :: CWire -> String
toName (CWire x _) = x

negateWires :: [CWire] -> [CWire]
negateWires wires = map negateWire wires

negateWire :: CWire -> CWire
negateWire (CWire x n) = (CWire x (-n))
negateWire (CInt  n)   = (CInt (-n))

--------------------------------------------------------------------------------
-- | Circuit to R1CS
--------------------------------------------------------------------------------
-- {-
-- INPUTS: a circuit 
-- OUTPUT: a rank-1 constraint system 
-- -}
circuitToR1CS :: Circuit -> R1CS
circuitToR1CS 
  Circuit{cPublicParams, cPrivateParams, cGates, cWires} = r1cs
  where
    empty_constraint :: CMap
    empty_constraint = Map.fromList $ (\v -> (toName v, [])) <$> cWires
    a = empty_constraint
    b = empty_constraint
    c = empty_constraint
    (_ ,(a', b', c')) = foldl (\(i,abc) gate -> (i+1, gateToConstraint i abc gate))  
                      (1, (a, b, c)) cGates
    r1cs :: R1CS
    r1cs = R1CS { r1csA  = a'
                , r1csB  = b'
                , r1csC  = c'
                , degree = toInteger $ length cPublicParams + length cGates + 1
                , r1csN  = toInteger $ length cPublicParams
                }

gateToConstraint :: Integer -> (CMap, CMap, CMap) -> CGate -> (CMap, CMap, CMap)
gateToConstraint i (a, b, c) (CGate CTimes left right out) = (a', b', c')
  where
    a' = R1CS.insertAll (wireToConstraint i <$> left)  a
    b' = R1CS.insertAll (wireToConstraint i <$> right) b
    c' = R1CS.insertAll (wireToConstraint i <$> [out]) c
    -- takes a gate number and a wire, returns a constraint
    wireToConstraint :: Integer -> CWire -> (String, (Integer, Integer))
    wireToConstraint n (CWire x i) = (x, (n, i))
    wireToConstraint n (CInt i)    = ("&1", (n, i))

gateToConstraint i (a, b, c) (CGate CDivide left right out) = (a', b', c')
  where -- l/r = o -> r * o = l
    a' = R1CS.insertAll (wireToConstraint i <$> right) a
    b' = R1CS.insertAll (wireToConstraint i <$> [out]) b
    c' = R1CS.insertAll (wireToConstraint i <$> left) c
    wireToConstraint :: Integer -> CWire -> (String, (Integer, Integer))
    wireToConstraint n (CWire x i) = (x, (n, i))

gateToConstraint i (a, b, c) (CNormalize left right out) = 
  gateToConstraint i (a, b, c) (CGate CTimes left [right] out)

gateToConstraint i (a, b, c) (CAssert op left right out) = 
  gateToConstraint i (a, b, c) (CGate op left right out)

--------------------------------------------------------------------------------
-- | Circuit to QAP-Witness
--------------------------------------------------------------------------------
-- {-
-- INPUTS: a circuit, public inputs, and witness (private inputs)
-- OUTPUT: QAP-Witness which extends witness and safisfies the circuit
-- -}

type Env a  = Map.Map String a

qapWitness :: Circuit -> Env Fr -> Env Fr -> Env Fr
qapWitness Circuit { cPublicParams, cPrivateParams, cGates, cWires } 
  publicInputs privateInputs = foldl gateToWitness env' cGates
    where
      env = Map.fromList $ (\(CWire x _) -> (x, 0)) <$> cWires
      env' = Map.union (Map.union publicInputs privateInputs) env


gateToWitness :: Env Fr -> CGate -> Env Fr
gateToWitness env (CGate CTimes left right out) = env'
  where
    left_val  = computeWires env left
    right_val = computeWires env right 
    out_val   = left_val * right_val
    env' = Map.insert (toName out) out_val env

gateToWitness env (CGate CDivide left right out) = env'
  where
    left_val  = computeWires env left
    right_val = computeWires env right 
    out_val   = left_val / right_val
    env' = Map.insert (toName out) out_val env

gateToWitness env (CNormalize left right out) = env''
  where
    left_val = computeWires env left
    m        = if left_val == 0 then 0 else 1/left_val
    out_val  = if left_val == 0 then 0  else 1
    env'  = Map.insert (toName right) m env
    env'' = Map.insert (toName out) out_val env'

gateToWitness env (CAssert _ _ _ _) = env -- do nothing


computeWires :: Env Fr -> [CWire] -> Fr
computeWires env wires = foldl (\acc wire -> acc + (compute_wire env wire)) 0 wires

compute_wire :: Env Fr -> CWire -> Fr
compute_wire env (CInt n) = fromInteger n
compute_wire env (CWire x n) = 
  case Map.lookup x env of
   Just v -> v * (fromInteger n)
   Nothing -> error $ "compute_wire - cannot find wire: " ++ x

--------------------------------------------------------------------------------
-- | Pretty Printer
--------------------------------------------------------------------------------

instance (Pretty a) => Pretty (ICircuit a) where
  pretty Circuit { cPublicParams, cPrivateParams, cGates, cWires } = 
            pretty "Public Parameters: " <+> sep (pretty <$> cPublicParams)  <>
    line <> pretty "Private Parameters:" <+> sep (pretty <$> cPrivateParams) <>
    line <> pretty "Wires" <+> align (vsep (pretty <$> cWires))              <>
    line <> pretty "Gates" <+> align (vsep (pretty <$> cGates))


instance (Pretty a) => Pretty (IGate a) where
  pretty (CGate op left_ins right_ins out) = pretty_gate op left_ins right_ins out "->"
  pretty (CNormalize left_ins right out) = pretty_gate CTimes left_ins [right] out "Normalize To"
  pretty (CAssert op left_ins right_ins out) = pretty_gate op left_ins right_ins out "="

pretty_gate op left_ins right_ins out arrow = pretty_wires left_ins <+> pretty op <+> pretty_wires right_ins <+> pretty arrow <+>pretty out

pretty_wires wires = if length wires > 1 
                      then pretty "(" <> (sep $ pretty <$> wires) <>pretty ")"
                      else sep $ pretty <$> wires

instance (Pretty a) => Pretty (IWire a) where
  pretty (CWire x n) = if n == 1 then pretty "<" <> pretty x <> pretty ">"
                        else pretty n <> pretty "<" <> pretty x <> pretty ">"
  pretty (CInt n)    = pretty n

instance Pretty CPrim2 where
  pretty CTimes  = pretty "*"
  pretty CDivide = pretty "/"

--------------------------------------------------------------------------------
-- | Pretty Printer - VCircuit
--------------------------------------------------------------------------------

-- Circuit with a value assigned to each variable
type VCircuit = ICircuit VId
type VWire = IWire VId
type VGate = IGate VId

data VId = VId Id Integer
  deriving (Show)

instance Pretty VId where
  pretty (VId x v) = pretty x <> pretty "[" <> pretty v <> pretty "]"

setCircuitValue :: Env Fr -> Circuit -> VCircuit
setCircuitValue env circuit = (setVId env) <$> circuit

setVId :: Env Fr -> Id -> VId
setVId env x = 
  case Map.lookup x env of
    Just v -> VId x (asInteger v)
    Nothing -> error $ "setVId can't find id " ++ x ++ " in environment" 

