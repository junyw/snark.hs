module Runner 
  (
    compile
  , runCompiler
  , runKeyGen
  , runProver
  , runVerifier
  , qapInst
  ) where 

import Field.Fr
import Compiler.Parser
import Compiler.Compiler
import Compiler.Circuit
import Compiler.R1CS
import Compiler.QAP
import Protocol

import Crypto.Random (MonadRandom)

import qualified Data.Map.Strict as Map
import System.IO

data Source = File FilePath
            | Code String 
  deriving (Show)

--------------------------------------------------------------------------------
-- | Run Compiler 
--------------------------------------------------------------------------------
runCompiler :: FilePath -> IO Circuit
runCompiler f = runCompiler' (File f)

runCompiler' :: Source -> IO Circuit
runCompiler' src = do
  code <- getSource src
  pure $ compile (getSourceName src) code


getSource :: Source -> IO String
getSource (Code code) = pure code
getSource (File f)    = readFile f

getSourceName :: Source -> String
getSourceName (Code _) = "undefined"
getSourceName (File f) = f

--------------------------------------------------------------------------------
-- | Run Protocol from Circuit
--------------------------------------------------------------------------------
qapInst :: Circuit -> QAP Fr
qapInst = r1csToQAP . circuitToR1CS 

runKeyGen :: (MonadRandom m) => Circuit -> m (ProvingKey, VerificationKey)
runKeyGen circuit = do 
    runKeyGenFromQAP qap 
  where 
    qap  = qapInst      circuit 

runProver :: (MonadRandom m) => Circuit -> ProvingKey -> [(String, Fr)] -> [(String, Fr)] -> m Proof
runProver circuit pk public private = do 
    runProverFromQAP qap pk qap_wit 
  where 
    qap  = qapInst      circuit 
    public_params = Map.fromList $ (\(x, y) -> ("&"++x, y)) <$> public
    private_params = Map.fromList private
    qap_wit = qapWitness circuit public_params private_params 

runVerifier :: VerificationKey -> Proof -> [(String, Fr)] -> Bool
runVerifier vk proof public = verifier vk proof public_params
  where
    public_params = Map.fromList $ (\(x, y) -> ("&"++x, y)) <$> public

runProtocol :: (MonadRandom m) => Circuit -> [(String, Fr)] -> [(String, Fr)] -> m Bool
runProtocol circuit public private = do 
    (pk, vk) <- runKeyGenFromQAP qap
    proof    <- runProverFromQAP qap pk qap_wit
    pure $ verifier vk proof public_params 
  where 
    qap  = qapInst      circuit 
    public_params = Map.fromList $ (\(x, y) -> ("&"++x, y)) <$> public
    private_params = Map.fromList private
    qap_wit = qapWitness circuit public_params private_params 

