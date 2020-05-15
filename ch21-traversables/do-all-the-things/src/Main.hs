module Main where

import Test.QuickCheck 
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import HTTPStuff
import QualityControl

main :: IO ()
main = do
  putStrLn "It ran, somehow!"
  putStrLn ""
  putStrLn "-------------*Identity*--------------"
  quickBatch (traversable trgId)
  putStrLn ""
  putStrLn "-------------*Constant*--------------"
  quickBatch (traversable trgConstant)
  putStrLn ""
  putStrLn "-------------*Optional*--------------"
  quickBatch (traversable trgOp)
  putStrLn ""
  putStrLn "---------------*List*----------------"
  quickBatch (traversable trgList)
  putStrLn ""
  putStrLn "---------------*Ski*-----------------"
  quickBatch (traversable trgSki)
  putStrLn ""
  putStrLn "---------------*Tree*----------------"
  quickBatch (traversable trgTree)
  putStrLn ""