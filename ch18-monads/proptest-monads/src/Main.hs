module Main where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import CustomMonads

main :: IO ()
main = do
  quickBatch (monad )