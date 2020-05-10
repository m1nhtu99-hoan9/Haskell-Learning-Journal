{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module ApplicativesInUse where
import Control.Applicative
import Data.List (elemIndex)
import Data.Semigroup as SM
import Data.Monoid as M

a = (,) <$> [1, 2] <*> [3, 4]
b = liftA2 (,) [1,2] [3,4]
areTheyEquals = (putStrLn . show) (a == b)

{-
Prelude> :t (,)
(,) :: a -> b -> (a, b)
-- (,) :: Num a, Num b => a -> b -> (a, b)

Prelude> :t (,) <$> [1,2]
(,) <$> [1,2] :: Num a => [b -> (a, b)]

--Instance Functor ((,) Num a) 
fmap (,) [1, 2] == [x1 -> (1, x1), x2 -> (2, x2)]

[x1 -> (1, x1), x2 -> (2, x2)] <*> [3,4] = [(1,3),(1,4),(2,3),(2,4)]
-}

-- EXERCISE: LOOKUPS
added :: Maybe String 
added = show <$> (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6]) 

y :: Maybe Integer 
y = lookup 3 $ zip [1,2,3] [4,5,6]
z :: Maybe Integer 
z = lookup 2 $ zip [1,2,3] [4,5,6]
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x0 :: Maybe Int 
x0 = elemIndex 3 [1..5]
y0 :: Maybe Int
y0 = elemIndex 4 [1..5]
max' :: Int -> Int -> Int
max' a b = getMax ((Max a) SM.<> (Max b)) 
maxed :: Maybe Int
maxed = max' <$> x0 <*> y0

xs = [1,2,3]
ys = [4,5,6]

x1 :: Maybe Integer 
x1 = lookup 3 $ zip xs ys
y1 :: Maybe Integer
y1 = lookup 2 $ zip xs ys
pairTupToList :: (a, a) -> [a]
pairTupToList (x, y) = [x, y]
sumMaybeInts :: [Maybe Integer] -> Maybe Integer
sumMaybeInts xs = foldr (\l r -> pure (+) <*> l <*> r) (Just 0) xs
summed :: Maybe Integer
summed = sumMaybeInts $ pairTupToList $ (,) x1 y1

-- EXERCISE: IDENTITY INSTANCE
newtype Identity a = Identity a 
                     deriving (Ord, Eq, Show)

instance Functor Identity where 
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x 
  (<*>) (Identity f) (Identity x) = Identity (f x) 

{-
*ApplicativesInUse> ushit = pure "ushit" :: Identity String
*ApplicativesInUse> fmap length ushit
Identity 5
*ApplicativesInUse> pure length <*> ushit
Identity 5
-} 

-- EXERCISE: CONSTANT INSTANCE
newtype Constant a b = Constant { getConstant :: a } 
                       deriving (Ord, Eq, Show)
-- type role Constant representational phantom

instance Functor (Constant a) where 
  fmap _ (Constant x) = Constant x -- throwing any function application away

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x M.<> y) 

-- Example for Applicative Maybe

data Cow = Cow {
  name :: String
, age :: Int
, weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing 
noEmpty s = Just s

noNegative :: Int -> Maybe Int 
noNegative n
  | n >= 0    = Just n
  | otherwise = Nothing

constructCow :: String -> Int -> Int -> Maybe Cow
constructCow n a w 
  = Cow <$> (noEmpty n) <*> (noNegative a) <*> (noNegative w)

-- EXERCISE: FIXER UPPER
a1 = const <$> Just "Hello" <*> pure "World"  
a2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3] 