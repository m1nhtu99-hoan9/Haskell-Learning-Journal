module CustomFoldableInstances where

import Data.Monoid
import Data.Foldable

{-
class Foldable (t :: * -> *) where
  [...]
  foldMap :: Monoid m => (a -> m) -> t a -> m
  [...]
-}

--question 1 
data Constant a b = Constant b 
                  deriving (Eq, Show)

instance Foldable (Constant a) where 
  foldMap _ _ = mempty

--question 2
data Two a b = Two a b 
               deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b 

--question 3 
data Three a b c = Three a b c 
                   deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

--question 4
data Three' a b = Three' a b b 
                  deriving (Show, Eq)

instance Foldable (Three' a) where
  foldMap f (Three' x y1 y2) = (f y1) <> (f y2)

--question 5
data Four' a b = Four' a b b b
                 deriving (Show, Eq)

instance Foldable (Four' a) where
  foldMap f (Four' x y1 y2 y3) = (f y1) <> (f y2) <> (f y3) 

--extra question 
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (g f) where
  g f a = if f a then pure a else mempty
-- `g` serves as a switch here: if `(f a)` is true, than switch `g` on
--                              else, switch `g` off; 
--  if `g` is switched on, it lifts the value it wraps around 1-typelevel up
--  if `g` is switched off, it evaluates to `mempty`