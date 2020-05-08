module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import qualified ValidateFunctorLaws as V
import qualified OtherFunctors as OF

main :: IO ()
main = do
  putStrLn "Any structure has kind * -> * that abide to the laws of Identity" 
  putStrLn "and Composabilty is also qualified to be a Functor."
  putStrLn "" 
  putStrLn "--------------------------------Do some warm-ups---------------------------------"
  putStrLn ""
  putStrLn "Do \"listIntToBool\" abide Identity law?"
  quickCheck (V.listIntToBoolAssoc)
  putStrLn ""
  putStrLn "Do f :: [Int] -> [Int] abide Composability law?"
  quickCheck (V.functorComposability' :: V.IntToIntAssoc)
  putStrLn ""
  putStrLn "-----------------------------Now do some real stuffs-----------------------------"
  putStrLn ""
  putStrLn "1.1.a. Do \"Identity\" applying to Int abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Identity Int -> Bool)
  putStrLn "1.1.b. Do \"Identity\" applying to Int abide Composability law?"
  quickCheck (V.functorComposability' :: OF.IdenIntAssoc)
  putStrLn ""
  putStrLn "1.2.a. Do \"Identity\" applying to String abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Identity String -> Bool)
  putStrLn "1.2.b. Do \"Identity\" applying to String abide Composability law?"
  quickCheck (V.functorComposability' :: OF.IdenStringAssoc)
  putStrLn ""
  putStrLn "1.3.a. Do \"Identity\" applying to Maybe String abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Identity (Maybe String) -> Bool)
  putStrLn "1.3.b. Do \"Identity\" applying to Maybe String abide Composability?"
  quickCheck (V.functorComposability' :: OF.IdenMaybeStringsAssoc)
  putStrLn ""
  putStrLn "2.1.a. Do \"Pair\" applying to Int abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Pair Int -> Bool)
  putStrLn "2.1.b. Do \"Pair\" applying to Int abide Composability law?"
  quickCheck (V.functorComposability' :: OF.PairIntAssoc)
  putStrLn ""
  putStrLn "2.2.a. Do \"Pair\" applying to String abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Pair String -> Bool)
  putStrLn "2.2.b. Do \"Pair\" applying to String abide Composability law?"
  quickCheck (V.functorComposability' :: OF.PairStringAssoc)
  putStrLn ""
  putStrLn "2.3.a. Do \"Pair\" applying to Maybe String abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Pair (Maybe String) -> Bool)
  putStrLn "2.3.b. Do \"Pair\" applying to Maybe String abide Composability?"
  quickCheck (V.functorComposability' :: OF.PairMaybeStringsAssoc)
  putStrLn ""
  putStrLn "3.1.a. Do \"Two\" applying to Trivial->Int abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Two OF.Trivial Int -> Bool)
  putStrLn "3.1.b. Do \"Two\" applying to Trivial->Int abide Composability?"
  quickCheck (V.functorComposability' :: OF.TwoIntAssoc)
  putStrLn ""
  putStrLn "3.2.a. Do \"Two\" applying to Int->String abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Two Int String -> Bool)
  putStrLn "3.2.b. Do \"Two\" applying to Int->String abide Composability law?"
  quickCheck (V.functorComposability' :: OF.TwoStringAssoc)
  putStrLn ""
  putStrLn "3.3.a. Do \"Two\" applying to String->(Maybe String) abide Identity law?"
  quickCheck (V.functorIdentity :: OF.Two String (Maybe String) -> Bool)
  putStrLn "3.3.b. Do \"Two\" applying to String->(Maybe String) abide Composability?"
  quickCheck (V.functorComposability' :: OF.TwoMaybeStringsAssoc)
  putStrLn ""
