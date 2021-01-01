{-# LANGUAGE FlexibleContexts #-}
module QualityControl where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative (liftA2)
import Data.Monoid (Sum, Product)

newtype Identity a = Identity a 
                   deriving (Show, Eq, Ord)
instance Functor Identity where
  fmap f (Identity x) = Identity (f x) 
instance Foldable Identity where
  foldMap f (Identity x) = f x
instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x -- have applicative behaviour

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq    
trgId :: Identity (String, Sum Int, Product Int)
trgId = undefined              

-- traverse :: (Traversable t, Applicative f) 
--          => (a -> f b) -> t a -> f (t b)
newtype Constant a b = Constant { getConstant :: a }
                       deriving (Show, Eq, Ord)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x
instance Foldable (Constant a) where
  foldMap _ _ = mempty
instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x --have applicative behaviour

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary 

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq    

trgConstant :: Constant [Int] (String, Sum Int, Product Int) 
trgConstant = undefined  

data Optional a = Nada
                | Yep a 
                deriving (Show, Eq, Ord)
instance Functor Optional where 
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep (f x)
instance Foldable Optional where
  foldMap _ Nada    = mempty --monoidal behaviour
  foldMap f (Yep a) = f a    --`f` has to evaluate to a monoid value
instance Traversable Optional where 
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = fmap Yep (f a)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    elements [Yep x, Nada]
instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq    
trgOp :: Optional (String, Sum Int, Product Int) 
trgOp = undefined  

data List a = Nil
            | Cons a (List a) 
            deriving (Show, Eq, Ord)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs
instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
   x <- arbitrary
   xs <- arbitrary
   elements [Nil, Cons x xs]
instance Eq a => EqProp (List a) where
  (=-=) = eq
trgList :: List (String, Sum Int, Product Int) 
trgList = undefined  


data Ski n a = Ski (n a) a 
             deriving (Show, Eq)

instance (Functor n) => Functor (Ski n) where
  fmap f (Ski x' x) = Ski (fmap f x') (f x)
instance (Foldable n) => Foldable (Ski n) where
  foldMap f (Ski x' x) = mappend (foldMap f x') (f x)
  -- x' itself is also a foldable instance value
instance (Traversable n) => Traversable (Ski n) where
  traverse f (Ski x' x) = liftA2 Ski (traverse f x') (f x)

instance (Functor n, Arbitrary (n a), Arbitrary a) 
         => Arbitrary (Ski n a) where
  arbitrary = liftA2 Ski arbitrary arbitrary     
instance (Eq (n a), Eq a) => EqProp (Ski n a) where
  (=-=) = eq
trgSki :: [] (Int, Product Int, String)
trgSki = undefined


data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty            = Empty
  fmap f (Leaf x)         = Leaf (f x)
  fmap f (Node x1 x2 x3)  = Node (f <$> x1) (f x2) (f <$> x3)   
instance Foldable Tree where
  foldMap _ Empty           = mempty
  foldMap f (Leaf x)        = f x
  foldMap f (Node x1 x2 x3) = mconcat [foldMap f x1, f x2, foldMap f x3]
instance Traversable Tree where
  traverse _ Empty            = pure Empty
  traverse f (Leaf x)         = Leaf <$> f x
  traverse f (Node x1 x2 x3)  = Node <$> traverse f x1 <*> f x2 <*> traverse f x3

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do 
    x1 <- arbitrary
    x2 <- arbitrary 
    x3 <- arbitrary
    elements [Node x1 x2 x3, Leaf x2, Empty]
instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

trgTree :: [] (Int, Product Int, String)
trgTree = undefined