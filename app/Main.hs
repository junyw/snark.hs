module Main where

import Lib

main :: IO ()
main = do

  circuit <- runCompiler "app/example.snark"
  putStrLn . show . pretty $ circuit             
  
  putStrLn "Run Setup"
  (pk, vk) <- runKeyGen circuit
  
  putStrLn "Run Prover"
  proof <- runProver circuit pk [("y", 35)] [("x", 3)]
  
  putStrLn "Run Verifier"
  if runVerifier vk proof [("y", 35)]
    then putStrLn "Proof is accepted" 
    else putStrLn "Proof is rejected"
    
  pure ()
