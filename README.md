# snark.hs

An experimental DSL for creating zero knowledge proofs (zk-SNARK) using Pinocchio [[PGHR13]](https://eprint.iacr.org/2013/279) protocol.

## Usage

Following an example from [Vitalik](https://medium.com/@VitalikButerin/quadratic-arithmetic-programs-from-zero-to-hero-f6d558cea649), 
the goal of the prover is to convince the verifier that they know a solution to the equation `x^3 + x + 5 == 35` without revealing it.

The entry point of the program is the `main` function, which takes public and private parameters as inputs and returns `true` if the inputs are accepted. All values are field integers. 

Save the program to a file `example.snark`
```python
def main(public y, private x): 
  y == x * x * x + x + 5
```

To run the protocol:

```haskell
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
```

## Implementation

- [x] Parser
- [ ] Type checking
- [x] Circuit generation
- [x] Circuit to R1CS
- [x] R1CS to QAP
- [x] Witness computation
- [x] PGHR protocol
- [x] BN128 curve operations
- [ ] Support for loops, comparisons, functions

