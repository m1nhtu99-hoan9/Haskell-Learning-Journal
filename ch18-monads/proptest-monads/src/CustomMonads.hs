module CustomMonads where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- CHAPTER EXERCISE: WRITE INSTANCES
--  notes: Applicative instance always need Functor instance
--         Monad instance always need Applicative instance
--  question 1
data Nope = Nope deriving (Show, Eq)
 
instance Functor Nope where
  fmap _ Nope = Nope

instance Applicative Nope where
  pure _  = Nope
  _ <*> _ = Nope  

instance Monad Nope where
  return = pure
  _ >>= _ = Nope