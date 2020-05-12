module CustomMonads where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- CHAPTER EXERCISE: WRITE INSTANCES
--  notes: Applicative instance always need Functor instance
--         Monad instance always need Applicative instance
--  QUESTION 1
data Nope a = Nope 
              deriving (Show, Eq)
--    `a` is phantom type
 
instance Functor Nope where
  fmap _ Nope = Nope
instance Applicative Nope where
  pure _  = Nope
  _ <*> _ = Nope  
instance Monad Nope where
  return = pure
  _ >>= _ = Nope

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return Nope
instance Eq a => EqProp (Nope a) where
  (=-=) = eq
trgNope :: Nope ([Int], String, Maybe String)
trgNope = undefined

-- QUESTION 2
data FlippedEither b a = Left' a 
                       | Right' b
                       deriving (Eq, Show)

instance Functor (FlippedEither b) where 
  fmap f (Left' x)  = Left' (f x)
  fmap f (Right' y) = Right' y

instance Applicative (FlippedEither b) where 
  pure                = Left' 
  Left' f  <*> Left' x  = Left' (f x)
  Right' y <*> _       = Right' y
  _       <*> Right' y = Right' y

instance Monad (FlippedEither b) where
  return        = pure
  Left' x  >>= f = f x
  Right' y >>= _ = Right' y

instance (Arbitrary a, Arbitrary b) => Arbitrary (FlippedEither b a) where 
  arbitrary = do
    y <- arbitrary
    x <- arbitrary
    elements [Left' x, Right' y]

instance (Eq a, Eq b) => EqProp (FlippedEither a b) where
  (=-=) = eq

trgFEither :: FlippedEither String ([Int], String, Maybe String)
trgFEither = undefined

-- QUESTION 3
newtype Identity a = Identity a
                     deriving (Ord, Eq, Show)

instance Functor Identity where 
  fmap f (Identity x) = Identity (f x)
instance Applicative Identity where 
  pure = Identity
  Identity f <*> Identity x = Identity (f x)
instance Monad Identity where
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  Identity x >>= f = (f x) 

instance (Arbitrary a) => Arbitrary (Identity a) where 
  arbitrary = do
    x <- arbitrary
    return (Identity x) 
instance Eq a => EqProp (Identity a) where
  (=-=) = eq
trgId :: Identity ([Int], String, Maybe String)
trgId = undefined

-- QUESTION 4
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

cons1 = Cons "ab" (Cons "cd" (Cons "efg" Nil))
cons2 = Cons "hijk" (Cons "lmno" Nil)
cons3 = Cons "qrs" Nil

instance Semigroup (List a) where
  Nil         <> x  = x
  (Cons x ls) <> ys = Cons x (ls <> ys)
  -- behave similarly to the conventional list
instance Monoid (List a) where 
  mempty            = Nil
  --`mconcat` will be [List a] -> List a, which is undesired
  -- we need `mconcat` alternative which is List (List a) -> List a

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons a as) = f a (fold f b as)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

instance Functor List where 
  fmap f Nil        = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

recurListToList :: (Eq a, Show a) => List a -> [a]
recurListToList Nil         = []
recurListToList (Cons x ls) = x : (recurListToList ls)

listToRecurList :: (Eq a, Show a) => [a] -> List a
listToRecurList []     = Nil
listToRecurList (x:xs) = Cons x (listToRecurList xs)


{-
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}
instance Applicative List where 
  pure x = Cons x Nil 
  Nil <*> _ = Nil
  _   <*> Nil = Nil
  --fs <*> xs = join (fmap (`fmap` xs) fs)
instance Arbitrary a => Arbitrary (List a) where 
  arbitrary = do
    r  <- arbitrary
    rs <- arbitrary
    elements [Nil, Cons r rs]

instance Eq a => EqProp (List a) where
  (=-=) = eq
trgList :: List (Int, Int, Int)
trgList = undefined

{-
∀ Functor f =>
    fmap     :: (a -> b) -> f a -> f b
(`fmap` f a) :: (a -> b) -> f b
-}