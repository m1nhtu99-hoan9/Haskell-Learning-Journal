module LibraryFunctions where

import Data.Monoid
import Data.Foldable

-- question 1
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . (foldMap Sum)
-- question 2
product :: (Foldable t, Num a) => t a -> a
product = getProduct . (foldMap Product)
-- question 3 
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . (foldMap (Any . (== x)))
-- question 4
data Least a = Least { getLeast :: Maybe a }
              deriving (Ord, Show, Eq)

instance (Ord a) => Monoid (Least a) where
  mempty = Least Nothing
    -- `a` need to be an instance of `Ord` to use `min`
  --  for all b, `a@(Maybe b)` is already an `Ord` instance
  mappend (Least (Just x)) (Least (Just y)) = Least (Just (min x y))
  mappend x                (Least Nothing)  = x 
  mappend (Least Nothing)  x                = x

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getLeast . (foldMap (Least . Just)) 

--question 5
data Most a = Most { getMost :: Maybe a }
              deriving (Ord, Show, Eq)

instance (Ord a) => Monoid (Most a) where
  mempty = Most Nothing
    -- `a` need to be an instance of `Ord` to use `min`
  --  for all b, `a@(Maybe b)` is already an `Ord` instance
  mappend (Most (Just x)) (Most (Just y)) = Most (Just (max x y))
  mappend x               (Most Nothing)  = x 
  mappend (Most Nothing)  x              = x

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMost . (foldMap (Most . Just)) 
-- in the first execution step of `foldMap`, no argument passed into `Just`
-- consequentially, `Most . Just` application will evaluate to `mempty`

--question 6
data Void a = Void { isVoid :: Bool }
              deriving (Show, Eq, Ord)
instance Monoid (Void a) where 
  mempty                          = Void True 
  mappend (Void True) (Void True) = Void True
  mappend  _           _          = Void False -- conjunction

null' :: (Foldable t) => t a -> Bool
null' = isVoid . (foldMap (Void . (const False)))
-- in the first execution step of `foldMap`, no argument passed into `const False`
-- consequentially, `Void . (const False)` application will evaluate to `mempty`

--question 7
data Count a = Count { getCount :: Int }
             deriving (Show, Eq, Ord)
instance Monoid (Count a) where 
  mempty                      = Count 0
  mappend (Count a) (Count b) = Count (a + b)

length' :: (Foldable t) => t a -> Int
length' = getCount . (foldMap (Count . (const 1)))
-- in the first execution step of `foldMap`, no argument passed into `const False`
-- consequentially, `Void . (const False)` application will evaluate to `mempty`

--question 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) mempty 

--question 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

--question 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f x = foldr (mappend . f) mempty x

