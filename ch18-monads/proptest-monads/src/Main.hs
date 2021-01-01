module Main where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import CustomMonads

main :: IO ()
main = do
  putStrLn "-------------------Property Tests for Custom Monads-------------------"
  putStrLn ""
  putStrLn "1. \"Nope\" Monad"
  quickBatch (monad trgNope)
  putStrLn ""
  putStrLn "2. \"FlippedEither\" Monad"
  quickBatch (monad trgFEither)
  putStrLn ""
  putStrLn "3. \"Identity\" Monad"
  quickBatch (monad trgId)
  putStrLn ""
  putStrLn "4. \"List\""
  quickBatch (monoid trgList)
  quickBatch (applicative trgList)
  quickBatch (monad trgList)