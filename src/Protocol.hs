{-
The PGHR zk-SNARK protocol

[PGHR13]  Bryan Parno, Craig Gentry, Jon Howell, Mariana Raykova. Pinocchio: Nearyly Practical Verifiable Computation. 2013. 
[BSCTV14] Eli Ben-Sasson, Alessandro Chiesa, Eran Tromer, and Madars Virza. Succinct non-interactive zero knowl-edge for a von Neumann architecture. 2014. 

-}
module Protocol 
  ( keyGen
  , prover
  , verifier
  , PublicInput(..)
  , PrivateInput(..)
  , ProvingKey(..)
  , VerificationKey(..)
  , Proof(..)
  , runKeyGenFromQAP
  , runProverFromQAP
  , runProtocolFromQAP
  ) where

import Field.Fr 
import Curve.Curve
import Curve.Pairing

import Util.Polynomial 
import Compiler.R1CS 
import Compiler.QAP

import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)

import qualified Data.Map.Strict as Map


{-
Given a QAP of size m, degree d, and number of public inputs n, 
the proving key, verification key, and proof are defined as:
-}

data ProvingKey = Pk { pkA  :: [G1] -- length m + 4
                     , pkA2 :: [G1] -- length m + 3 - n
                     , pkB  :: [G2] -- length m + 4
                     , pkB2 :: [G1] -- length m + 4
                     , pkC  :: [G1] -- length m + 4
                     , pkC2 :: [G1] -- length m + 4
                     , pkK  :: [G1] -- length m + 4
                     , pkH  :: [G1] -- length d + 1
                     }
  deriving (Show)

data VerificationKey = Vk { vkA   :: G2
                          , vkB   :: G1
                          , vkC   :: G2
                          , vkg   :: G2
                          , vkbg1 :: G1
                          , vkbg2 :: G2
                          , vkZ   :: G2
                          , vkIC  :: [G1] -- length n + 1
                          } 
  deriving (Show)

data Proof = Proof { piA  :: G1
                   , piA2 :: G1
                   , piB  :: G2
                   , piB2 :: G1
                   , piC  :: G1
                   , piC2 :: G1
                   , piK  :: G1
                   , piH  :: G1
                   } 
  deriving (Show)


--------------------------------------------------------------------------------
-- | Key Generator 
--------------------------------------------------------------------------------

{-
INPUTS: a quadratic arithmetic program, randomly sampled elements from Fr
OUTPUT: a proving key pk and a verification key vk
-}

keyGen :: QAP Fr -> (Fr, Fr, Fr, Fr, Fr, Fr, Fr, Fr) -> (ProvingKey, VerificationKey)
keyGen QAP{ sizeOfPublicInputs, degreeOfZ, qapA, qapB, qapC, qapZ } 
       (tau, rhoA, rhoB, alphaA, alphaB, alphaC, beta, gamma) = 
  
  let n = sizeOfPublicInputs
      d = degreeOfZ
  
      -- 1. extend a, b, c
  
      -- Map.elems returns all elements of the map in the ascending order of their keys.
      -- special input &1 and public inputs are in the front followed by verifier inputs
      a_ext = Map.elems qapA ++ [qapZ, 0, 0] -- length m + 4
      b_ext = Map.elems qapB ++ [0, qapZ, 0] -- length m + 4
      c_ext = Map.elems qapC ++ [0, 0, qapZ] -- length m + 4

      a_tau = map (`eval` tau) a_ext
      b_tau = map (`eval` tau) b_ext
      c_tau = map (`eval` tau) c_ext
  
      a_rhoA             = map (rhoA*) a_tau
      a_alphaA_rhoA      = map (alphaA*) a_rhoA
      b_rhoB             = map (rhoB*) b_tau
      b_alphaB_rhoB      = map (alphaB*) b_rhoB
      rhoA_rhoB          = rhoA * rhoB
      c_rhoA_rhoB        = map (rhoA_rhoB*) c_tau
      c_alphaC_rhoA_rhoB = map (alphaC*) c_rhoA_rhoB
      combined           = zipWith3 (\x y z -> beta * (x + y + z)) a_rhoA b_rhoB c_rhoA_rhoB
      a_rhoA_g1          = map (g1 `scalarFr`) a_rhoA
      
      -- 2. create public key
      pk = Pk { pkA  = a_rhoA_g1
              , pkA2 = map (g1 `scalarFr`) (drop (fromIntegral $ n + 1) a_alphaA_rhoA)
              , pkB  = map (g2 `scalarFr`) b_rhoB
              , pkB2 = map (g1 `scalarFr`) b_alphaB_rhoB
              , pkC  = map (g1 `scalarFr`) c_rhoA_rhoB
              , pkC2 = map (g1 `scalarFr`) c_alphaC_rhoA_rhoB
              , pkK  = map (g1 `scalarFr`) combined
              , pkH  = map (\i -> (g1 `scalarFr` (tau^i))) [0..d]
              }
      
      -- 4. create verification key
      vk = Vk { vkA = g2 `scalarFr` alphaA
              , vkB = g1 `scalarFr` alphaB
              , vkC = g2 `scalarFr` alphaC
              , vkg = g2 `scalarFr` gamma
              , vkbg1 = g1 `scalarFr` (gamma * beta)
              , vkbg2 = g2 `scalarFr` (gamma * beta)
              , vkZ = g2 `scalarFr` ((qapZ `eval` tau) * rhoA * rhoB)
              , vkIC = take (fromIntegral $ n + 1) a_rhoA_g1
              }
  in (pk, vk)
  

--------------------------------------------------------------------------------
-- | Prover
--------------------------------------------------------------------------------

{- 
INPUTS: 
  a quadratic arithmetic program  proving key pk, 
  an assignment of all inputs (size = m + 1). The special input &1 is assigned to 1, followed by 
    public inputs &var_1 to &var_n and verifier inputs var_n+1 to var_m
  randomly sampled values from Fr
OUTPUT: proof
-}
type Env a  = Map.Map String a
type PublicInput a  = Map.Map String a
type PrivateInput a = Map.Map String a
type QAPWitness a   = Map.Map String a

prover :: QAP Fr -> ProvingKey -> QAPWitness Fr -> (Fr, Fr, Fr) -> Proof
prover QAP{ sizeOfPublicInputs, degreeOfZ, qapA, qapB, qapC, qapZ } 
       Pk { pkA, pkA2, pkB, pkB2, pkC, pkC2, pkK, pkH }
       qapwitness (delta1, delta2, delta3) =
  let 
    n = sizeOfPublicInputs
    vals = [1] ++ (Map.elems qapwitness) 
    
    -- 1. compute h = [h0, h1, ..., hd], which are the coefficients of 
    --    H(x) = (A(x) * B(x) - C(x))/Z(x) 

    z_delta1 = qapZ `scale` delta1
    z_delta2 = qapZ `scale` delta2
    z_delta3 = qapZ `scale` delta3

    a_poly = innerPolyFr (Map.elems qapA) vals + z_delta1
    b_poly = innerPolyFr (Map.elems qapB) vals + z_delta2
    c_poly = innerPolyFr (Map.elems qapC) vals + z_delta3
    
    (h_poly, _) = ((a_poly * b_poly) - c_poly) `divmod` qapZ 
    h_vec = reverse $ toList (norm h_poly)

    -- 2. set pkA' and pkA2'
    -- pkA' is same as pkA, but with 0,1,..,n terms mapped to zero.
    -- pkA2' is same as pkA2, but prepend n + 1 zeros. length = m + 4

    pkA' = (replicate (fromIntegral $ n+1) pointInf) ++ (drop (fromIntegral $ n+1) pkA)
    pkA2' = (replicate (fromIntegral $ n+1) pointInf) ++ pkA2

    -- 3. set c = [1, s1, ..., sm, delta1, delta2, delta3], length m + 4
    --    where s1 to sm are values of public and verifier inputs
    c_vec = vals ++ [delta1, delta2, delta3]
    
    -- 4. create proof
  in Proof { piA  = innerG1 pkA'  c_vec
           , piA2 = innerG1 pkA2' c_vec
           , piB  = innerG2 pkB   c_vec
           , piB2 = innerG1 pkB2  c_vec
           , piC  = innerG1 pkC   c_vec
           , piC2 = innerG1 pkC2  c_vec
           , piK  = innerG1 pkK   c_vec
           , piH  = innerG1 pkH   h_vec
           }


--------------------------------------------------------------------------------
-- | Verifier
--------------------------------------------------------------------------------

{-
INPUTS: verification key vk, proof, public input [x1, ...xn]
OUTPUT: True if proof is accepted, False if rejected
-}
pairing = reducedPairing

verifier :: VerificationKey -> Proof -> PublicInput Fr -> Bool
verifier 
  Vk{ vkA, vkB, vkC, vkg, vkbg1, vkbg2, vkZ, vkIC } 
  Proof{ piA, piA2, piB, piB2, piC, piC2, piK, piH } input = 
  let 
      -- 1. compute vkx
      vkx = innerG1 vkIC ([1] ++ Map.elems input) -- length n + 1
      
      -- 2. compute validity of knowledge commitments for A, B, C
      a = pairing piA vkA == pairing piA2 g2
      b = pairing vkB piB == pairing piB2 g2
      c = pairing piC vkC == pairing piC2 g2
      
      -- 3. check same coefficients were used
      d = pairing piK vkg == (pairing (vkx `pointAdd` piA `pointAdd` piC) vkbg2) * (pairing vkbg1 piB)
      
      -- 4. check QAP divisibility
      e = pairing (vkx `pointAdd` piA) piB == (pairing piH vkZ) * (pairing piC g2)

  in a && b && c && d && e


--------------------------------------------------------------------------------
-- | Runners
--------------------------------------------------------------------------------

runKeyGenFromQAP :: (MonadRandom m) => QAP Fr -> m (ProvingKey, VerificationKey)
runKeyGenFromQAP qap = do
  tau    <- random
  rhoA   <- random
  rhoB   <- random
  alphaA <- random
  alphaB <- random
  alphaC <- random
  beta   <- random
  gamma  <- random
  
  pure $ keyGen qap (tau, rhoA, rhoB, alphaA, alphaB, alphaC, beta, gamma)

runProverFromQAP :: (MonadRandom m) => QAP Fr -> ProvingKey -> QAPWitness Fr -> m Proof
runProverFromQAP qap pk qapwitness = do
  delta1 <- random
  delta2 <- random
  delta3 <- random
  pure $ prover qap pk qapwitness (delta1, delta2, delta3)

runProtocolFromQAP :: (MonadRandom m) => QAP Fr -> PublicInput Fr -> QAPWitness Fr -> m Bool
runProtocolFromQAP qap public_inputs qapwitness = do
  (pk, vk) <- runKeyGenFromQAP qap
  proof    <- runProverFromQAP qap pk qapwitness
  pure $ verifier vk proof public_inputs
