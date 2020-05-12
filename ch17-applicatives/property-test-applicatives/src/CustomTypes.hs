module CustomTypes where 

import Control.Applicative
import Control.Applicative (ZipList)
import Data.Monoid 
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo
            deriving (Show, Eq)

instance Arbitrary Bull where
  arbitrary = 
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools

instance Semigroup Bull where
  _ <> _ = Fools

instance EqProp Bull where (=-=) = eq
-- the function `eq` exported by `checkers` reuses 
--  the pre-existing Eq instance for the datatype 

type SSI = (String, String, Int)
trg :: [SSI]
trg = undefined

-- Applicative ZipList {not tested yet}

--  attempt to declare orphan instances
instance Monoid a => Monoid (ZipList a) where 
  mempty = pure mempty
instance Semigroup a => Semigroup (ZipList a) where
  x <> y = liftA2 (<>) x y 

{-
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary
instance Arbitrary a => Arbitrary (Product a) where
  arbitrary = Product <$> arbitrary
instance Eq a => EqProp (ZipList a) where
  (=-=) = eq
-}

-- From Chapter 18, just want to test it here
data Sum' a b = 
    First' a 
  | Second' b
  deriving (Show, Eq)

instance Functor (Sum' a) where
  fmap f (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

instance Applicative (Sum' a) where
  pure = Second' 
  (Second' x1) <*> (Second' y1) = Second' (x1 y1)
  (First' x1)  <*> _            = (First' x1)
  _            <*> (First' x1)  = (First' x1)

instance Monad (Sum' a) where 
  return = pure
  First' x1 >>= _  = First' x1 --nothing happen
  Second' y1 >>= f = f y1

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [First' a, Second' b]

instance (Eq a, Eq b) => EqProp (Sum' a b) where (=-=) = eq

trgSum :: Sum' [Int] (Maybe String, String, [Int])
trgSum = undefined

-- the triggers for `applicative`, `functor` , `monoid`, `monad`
-- behave a bit trickily
-- functor :: (Functor m, ...) => m (a, b, c) -> TestBatch