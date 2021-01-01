module Main where

import HTTPStuff ()
import QualityControl
  ( trgConstant,
    trgId,
    trgList,
    trgOp,
    trgSki,
    trgTree,
  )
import Test.QuickCheck ()
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (traversable)

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