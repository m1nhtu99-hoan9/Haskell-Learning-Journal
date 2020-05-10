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
