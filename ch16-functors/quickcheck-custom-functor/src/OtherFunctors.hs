module OtherFunctors where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Function
import qualified ValidateFunctorLaws as V

type FunMaybeStrings = Fun (Maybe String) (Maybe String)

-- QUESTION 1
newtype Identity a = Identity a 
                     deriving (Show, Eq)
instance Functor Identity where
--fmap :: (a -> a) -> Indentity a -> Identity a
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where 
  arbitrary = do
    r <- arbitrary
    return (Identity r)

type IdenIntAssoc = Identity Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
type FunStrings = Fun String String 
type IdenStringAssoc = Identity String -> FunStrings -> FunStrings -> Bool
type IdenMaybeStringsAssoc = Identity (Maybe String) -> FunMaybeStrings 
                             -> FunMaybeStrings -> Bool

-- QUESTION 2
data Pair a = Pair a a 
              deriving (Show, Eq)
instance Functor Pair where
  fmap f (Pair x1 x2) = Pair (f x1) (f x2)
  -- can be test by checking `fmap (*2) (Pair 45 54) :: Pair Int`

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    r1 <- arbitrary
    r2 <- arbitrary
    return (Pair r1 r2)  

type PairIntAssoc = Pair Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
type PairStringAssoc = Pair String -> (Fun String String) -> (Fun String String) 
                       -> Bool
type PairMaybeStringsAssoc = Pair (Maybe String) -> FunMaybeStrings 
                             -> FunMaybeStrings -> Bool

-- QUESTION 3 
data Two a b = Two a b
               deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two x1 x2) = Two x1 (f x2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do 
    x1 <- arbitrary
    x2 <- arbitrary
    return (Two x1 x2)

type TwoIntAssoc = Two Trivial Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
type TwoStringAssoc = Two Int String -> (Fun String String) 
                      -> (Fun String String) -> Bool
type TwoMaybeStringsAssoc = Two String (Maybe String) -> FunMaybeStrings
                             -> FunMaybeStrings -> Bool

-- QUESTION 4
data Three a b c = Three a b c
                   deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x1 x2 x3) = Three x1 x2 (f x3)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do 
    x1 <- arbitrary
    x2 <- arbitrary
    x3 <- arbitrary
    return (Three x1 x2 x3)

type ThreeIntAssoc = Three Trivial Trivial Int -> (Fun Int Int) 
                      -> (Fun Int Int) -> Bool
type ThreeStringAssoc = Three Trivial Int String -> (Fun String String) 
                        -> (Fun String String) -> Bool
type ThreeMaybeStringsAssoc = Three Int String (Maybe String) -> FunMaybeStrings
                             -> FunMaybeStrings -> Bool

-- QUESTION 5
data Three' a b = Three' a b b
                  deriving (Show, Eq)
instance Functor (Three' a) where 
  fmap f (Three' x1 x2 x3) = Three' x1 (f x2) (f x3)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x1 <- arbitrary 
    x2 <- arbitrary 
    x3 <- arbitrary 
    return (Three' x1 x2 x3)

type ThreeIntAssoc' = Three' Trivial Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
type ThreeStringAssoc' = Three' Trivial String -> (Fun String String) 
                         -> (Fun String String) -> Bool
type ThreeMaybeStringsAssoc' = Three' Int (Maybe String) -> FunMaybeStrings
                             -> FunMaybeStrings -> Bool

-- QUESTION 6: similar to QUESTION 4
data Four a b c d = Four a b c d 

-- QUESTION 7: similar to QUESION 5
data Four' a b = Four' a a b b 
                 deriving (Show, Eq)
instance Functor (Four' a) where 
  fmap f (Four' x1 x2 x3 x4) = Four' x1 x2 (f x3) (f x4)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x1 <- arbitrary   
    x2 <- arbitrary 
    x3 <- arbitrary 
    x4 <- arbitrary 
    return (Four' x1 x2 x3 x4)

type FourIntAssoc' = Four' Trivial Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
type FourStringAssoc' = Four' Trivial String -> (Fun String String) 
                         -> (Fun String String) -> Bool
type FourMaybeStringsAssoc' = Four' Int (Maybe String) -> FunMaybeStrings
                             -> FunMaybeStrings -> Bool

-- QUESTION 8
data Trivial = Trivial deriving (Eq, Show)
-- can't have Functor instance because this shit has Constant Kind *

instance Arbitrary Trivial where
  arbitrary = return Trivial 
  