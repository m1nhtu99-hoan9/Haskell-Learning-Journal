module Main where

import EitherT
import StateT
import ReaderT
import MonadTrans

main :: IO ()
main = do
    let printEitherT mm = print $ runEitherT mm
    printEitherT $ (*2) <$> x0
    printEitherT $ (*2) <$> x1
    printEitherT $ xf0 <*> x0
    printEitherT $ xf0 <*> x1
    printEitherT $ swapEitherT x1

-- test data

x0 :: EitherT String Maybe Int
x0 = pure 8

x1 :: EitherT String Maybe Int
x1 = EitherT (Just . Left $ "Nothing to be scared for")

xf0 :: EitherT String Maybe (Int -> String)
xf0 = pure (concat . flip replicate "uchihahaha")