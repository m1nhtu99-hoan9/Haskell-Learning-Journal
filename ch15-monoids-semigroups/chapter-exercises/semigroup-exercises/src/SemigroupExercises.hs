module SemigroupExercises where

import Data.Monoid
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) 
               => m -> m -> m -> Bool
semigroupAssoc a b c = 
  (==) ((a <> b) <> c) (a <> (b <> c))

-- QUESTION 1 
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

-- QUESTION 2
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

-- QUESTION 3
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

-- QUESTION 4
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

-- QUESTION 5
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

-- QUESTION 6 
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

-- QUESTION 7 
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _              <> _              = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary :: Gen Bool
    return (BoolDisj x)

type BoolDisjAssoc =  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- QUESTION 8 
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  _ <> Snd x     = Snd x
  Fst _ <> Fst x = Fst x
  Snd x <> _     = Snd x

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do 
  a <- arbitrary
  b <- arbitrary
  elements [Fst a, Snd b] -- generate one of the given value in the list, has type [t] -> Gen t

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrStrings = Or String String
type OrAssocString = OrStrings -> OrStrings -> OrStrings -> Bool

-- QUESTION 9
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Show a, Show b) => Show (Combine a b) where 
  show _ = "\"Combine\" with a function inside. \n Whatcha expect?"

--  Combine-as-semigroup doesn't need `Eq` instance because it doesn't
--    have identity unit yet.
 
instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

--  CoArbitrary signifies that the random `b` chosen by QuickCheck need to be reducible to `a`  
combGen:: (CoArbitrary a, Arbitrary b) => Gen (Combine a b) 
combGen = do 
  f <- arbitrary        
  return (Combine f) 

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where 
  arbitrary = combGen

type CombStrings = Combine String String  
combAssoc :: CombStrings -> CombStrings -> CombStrings -> String -> Bool 
combAssoc cs1 cs2 cs3 arg = 
  let leftAssocVector  = (cs1 <> cs2) <> cs3
      rightAssocVector = cs1 <> (cs2 <> cs3)
  in (==) (unCombine leftAssocVector arg) (unCombine rightAssocVector arg)

--QUESTION 10
newtype Comp a = Comp { unComp :: (a -> a) } 

instance Show a => Show (Comp a) where
  show _ = "\"Comp\" with a function inside. \n Whatcha expect?"

instance Semigroup a => Semigroup (Comp a) where 
  Comp f <> Comp g = Comp (\x -> f x <> g x)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where 
  -- for all the types `a` have both `CoArbitrary` instance and `Arbitrary` instance
  arbitrary = do
    f <- arbitrary
    return (Comp f)

type CompStrings = Comp String   
compAssoc :: CompStrings -> CompStrings -> CompStrings -> String -> Bool 
compAssoc cs1 cs2 cs3 arg = 
  let leftAssocVector  = (cs1 <> cs2) <> cs3
      rightAssocVector = cs1 <> (cs2 <> cs3)
  in (==) (unComp leftAssocVector arg) (unComp rightAssocVector arg)

{-
  This chapter's exercises are too long. 
  Not finished yet.
  May pick up later. 
-}




  

