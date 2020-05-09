{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

data BinaryTuple a b = BinaryTuple a b
                       deriving (Eq, Show)
  
newtype Flip t a b = Flip (t b a)
                     deriving (Eq, Show)

instance Functor (Flip BinaryTuple b) where
  fmap f (Flip (BinaryTuple a b)) = 
    Flip (BinaryTuple (f a) b)

getTupleValues :: BinaryTuple a b -> (a, b)
getTupleValues (BinaryTuple x y) = (x, y)
