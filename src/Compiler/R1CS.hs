module Compiler.R1CS 
  ( CMap
  , R1CS(..)
  , insert
  , insertAll
  ) where

import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc

import Field.Fr
import Util.Polynomial


--------------------------------------------------------------------------------
-- | Rank-1 Constraint System 
--------------------------------------------------------------------------------

type CMap = Map.Map String [(Integer, Integer)]

data R1CS = R1CS { r1csA  :: !CMap
                 , r1csB  :: !CMap
                 , r1csC  :: !CMap
                 , degree :: Integer -- number of Public Inputs + number of gates + number of outputs(0) + 1
                 , r1csN  :: Integer -- number of public inputs
                 } 
  deriving (Show)

insert :: (String, (Integer, Integer)) -> CMap -> CMap
insert (x, c) = Map.insertWith (++) x [c]

insertAll :: [(String, (Integer, Integer))] -> CMap -> CMap
insertAll cs m = foldr insert m cs

--------------------------------------------------------------------------------
-- | Pretty Printer
--------------------------------------------------------------------------------
instance Pretty R1CS where
  pretty R1CS { r1csA, r1csB, r1csC, degree, r1csN } = 
    pretty "Number of public inputs:" <+> pretty r1csN <>
    pretty ", Degree:" <+> pretty degree <>
    line <> pretty "A:" <+> pretty r1csA <>
    line <> pretty "B:" <+> pretty r1csB <>
    line <> pretty "C:" <+> pretty r1csC 

instance Pretty CMap where
  pretty m = align . sep $ (pretty_elem <$> ls)
    where 
      ls = Map.assocs m 
      pretty_elem (k, xs) = pretty k <+> pretty "[" <> sep (punctuate comma (pretty_pair <$> xs)) <> pretty "]"
      pretty_pair (x, y) = pretty "(" <> pretty x <> pretty "," <> pretty y <> pretty ")"





