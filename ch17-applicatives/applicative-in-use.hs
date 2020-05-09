{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module ApplicativesInUse where
import Control.Applicative
import Data.List (elemIndex)
import Data.Semigroup as SM

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
max' a b = getMax ((Max a) <> (Max b)) 
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