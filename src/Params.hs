module Params (
  b, 
  k,
  t,
  p,
  r,
  beta,
) where

--------------------------------------------------------------------------------
-- | Barreto-Naehrig parametrization
--------------------------------------------------------------------------------
t = 4965661367192848881
p = 21888242871839275222246405745257275088696311157297823662689037894645226208583
r = 21888242871839275222246405745257275088548364400416034343698204186575808495617

--------------------------------------------------------------------------------
-- | Curve parameter
--------------------------------------------------------------------------------
b = 3  -- E : y^2 = x^3 + b

--------------------------------------------------------------------------------
-- | Field parameter
--------------------------------------------------------------------------------
k = 12 -- embedding degree

beta :: Integer
beta = -1

