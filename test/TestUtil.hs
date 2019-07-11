module TestUtil 
 ( 
 -- * Rules for arithmetic operations
   commutes
 , associates
 , distributes
 , isIdentity
 , isInverse

 -- * Create compiler tests
 , testCompiler
 ) where 

import Field.Fr
import Compiler.Parser
import Compiler.Compiler
import Compiler.Circuit
import Compiler.R1CS
import Compiler.QAP
import Protocol
import Runner

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map
import Crypto.Random (MonadRandom)
import System.IO

--------------------------------------------------------------------------------
-- | Rules 
--------------------------------------------------------------------------------
commutes :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutes op x y = (x `op` y) == (y `op` x)

associates
 :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associates op x y z
  = (x `op` (y `op` z)) == ((x `op` y) `op` z)

distributes 
  :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributes mult add x y z
  = x `mult` (y `add` z) == (x `mult` y) `add` (x `mult` z)

isIdentity
  :: Eq a => (a -> a -> a) -> a -> a -> Bool
isIdentity op e x
  = (x `op` e == x) && (e `op` x == x)

isInverse 
  :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
isInverse op inv e x 
  = (x `op` inv x == e) && (inv x `op` x == e) 

--------------------------------------------------------------------------------
-- | Construct Tests
--------------------------------------------------------------------------------

testCompiler :: Bool -> (String, FilePath, [Fr], [Fr]) -> TestTree
testCompiler testAll (name, f, public, private) = 
  testCaseSteps name $ \step -> do 
    let in_file  = "test/input/" ++ f ++ ".snark"
    let out_file = "test/output/" ++ f ++ ".log"
    withFile out_file WriteMode (\handle -> do

      hPutStrLn handle ("public inputs: " ++ (show public))
      hPutStrLn handle ("private inputs: " ++ (show private))

      src <- readFile in_file
      sep handle "Source"   
      hPutStrLn handle src

      step "Generate Circuit"
      
      let ast = parse f src
      sep handle "AST"   
      hPutStrLn handle (show . pretty $ ast)

      let tagged = tag ast
      sep handle "TAG"
      hPutStrLn handle (show  $ tagged)

      let anf = atag. anormal . rename $ tagged
      sep handle "ANF"
      hPutStrLn handle (show . pretty $ anf)

      let circuit = genCircuit anf
      sep handle "Circuit"
      hPutStrLn handle $ show . pretty $ circuit


      step "Compute QAPWitness"
      let public_params  = Map.fromList $ (\(x, y) -> (toName x, y)) <$> (zip (cPublicParams circuit) public)
      let private_params = Map.fromList $ (\(x, y) -> (toName x, y)) <$> (zip (cPrivateParams circuit) private)
      let qap_wit = qapWitness circuit public_params private_params 
      sep handle "QAPWitness"
      hPutStrLn handle $ show qap_wit

      hPutStrLn handle $ show . pretty $ setCircuitValue qap_wit circuit

      step "Convert to QAP"
      let qap = qapInst circuit
      let sat = qapSat qap qap_wit
      sep handle "QAP is satisified by QAP-Witness"
      hPutStrLn handle $ show sat

      if testAll 
        then do 
          step "Run Setup"
          (pk, vk) <- runKeyGenFromQAP qap 

          step "Run Prover"
          proof <- runProverFromQAP qap pk qap_wit

          step "Run Verifier"
          let decision = verifier vk proof public_params 
          sep handle "Proof is Accepted"
          hPutStrLn handle $ show decision

          assertBool "Proof is Accepted" decision

        else do 
          assertBool "QAPWitness is Correct" sat)
  where 
    sep :: Handle -> String -> IO ()
    sep handle label= do 
      hPutStrLn handle "--------------------------------------------------------------------------------"
      hPutStrLn handle ("-- " ++ label)
      hPutStrLn handle "--------------------------------------------------------------------------------"
      return ()



