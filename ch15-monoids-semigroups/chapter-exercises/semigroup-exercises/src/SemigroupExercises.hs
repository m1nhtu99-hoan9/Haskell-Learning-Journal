module SemigroupExercises where

import Data.Monoid
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) 
               => m -> m -> m -> Bool
semigroupAssoc a b c = 
  (==) ((a <> b) <> c) (a <> (b <> c))

-- Question 1 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

{- 
  after built and exec'ed `stack exec semigroup`: 
    `+++ OK, passed 100 tests.`
-}

-- Question 2
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where 
  (Identity x1) <> (Identity x2) = Identity (x1 <> x2)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdenAssocString = 
  (Identity String) -> (Identity String) -> (Identity String) -> Bool

-- Question 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 x2) <> (Two y1 y2) = Two (x1 <> y1) (x2 <> y2)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoStrings = (Two String String)  
type TwoAssocString = 
  TwoStrings -> TwoStrings -> TwoStrings -> Bool

-- Question 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) 
  => Semigroup (Three a b c) where
    (Three x1 x2 x3) <> (Three y1 y2 y3) = Three (x1 <> y1) (x2 <> y2) (x3 <> y3)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) 
  => Arbitrary (Three a b c) where
    arbitrary = threeGen

type ThreeStrings = (Three String String String)  
type ThreeAssocString = 
  ThreeStrings -> ThreeStrings -> ThreeStrings -> Bool

-- Question 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) 
  => Semigroup (Four a b c d) where
    (Four x1 x2 x3 x4) <> (Four y1 y2 y3 y4) = 
      Four (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
  => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
  => Arbitrary (Four a b c d) where
    arbitrary = fourGen

type FourStrings = (Four String String String String)  
type FourAssocString = 
  FourStrings -> FourStrings -> FourStrings -> Bool

-- Question 6 
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

--    define the conjunction binary operation inside 
--    `Semigroup` typeclass instance of `BoolConj`
instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _             <> _             = BoolConj False

--    specify the generator for this type
boolConjGen :: Gen BoolConj
boolConjGen = do
  x <- arbitrary :: Gen Bool    --colloquially speaking, `Bool <- Arbitrary`
  return (BoolConj x)

--    define the `Arbitrary` typeclass instance of `BoolConj`
instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
