module Main where

import Test.QuickCheck
import SemigroupExercises

main :: IO ()
main = do
  putStrLn "-------------------------------------------------"
  putStrLn "Test associativity for \"Trivial\":"
  quickCheck (semigroupAssoc :: TrivAssoc)
  putStrLn ""
  putStrLn "Test associativity for \"Identity\" on String:"
  quickCheck (semigroupAssoc :: IdenAssocString)
  putStrLn ""
  putStrLn "Test associativity for \"Two\" on String:" 
  quickCheck (semigroupAssoc :: TwoAssocString)
  putStrLn ""
  putStrLn "Test associativity for \"Three\" on String:" 
  quickCheck (semigroupAssoc :: ThreeAssocString)
  putStrLn ""
  putStrLn "Test associativity for \"Four\" on String:" 
  quickCheck (semigroupAssoc :: FourAssocString)
  putStrLn ""
  putStrLn "Test associativity for \"BoolConj\":" 
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn ""
  putStrLn "Test associativity for \"BoolDisj\":" 
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  putStrLn ""
  putStrLn "Test associativity for \"Or\":" 
  quickCheck (semigroupAssoc :: OrAssoc)
  putStrLn ""
  putStrLn "-------------------------------------------------"