module ReaderPractice where
  
import Control.Applicative ( liftA2 )
import Data.Maybe ( Maybe(..), fromMaybe ) 
import Data.Traversable ( sequenceA )
import Data.Tuple ( uncurry )

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
-- zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
xs = lookup 3 (zip x y)
ys = lookup 6 (zip y z)
zs = lookup 4 (zip x y)
z' = flip lookup (zip x z) 

x1 :: Maybe (Integer, Integer)
x2 :: Maybe (Integer, Integer)
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x1 = (,) <$> xs <*> ys -- Just (6,9)
x2 = (,) <$> ys <*> zs -- Nothing
x3 = liftA2 (,) z' z'  -- x3 3 = (Just 9, Just 9)

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys) -- Just 15

-- uncurry :: (a -> b -> c) -> (a, b) -> c

summed :: Num c => (c, c) -> c
summed = liftA2 (+) fst snd 

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

main :: IO ()
main = do 
    print $ 
      sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
    --               t ~ []; f ~ (->) a; a ~ Integer
    print $ sequenceA [(> 3), (< 8), even] 7
    print $ sequA . fromMaybe 0 $ s'
    -- fold the boolean conjunction operator over the list of results of 
    -- `sequA` (applied to some value)
    print $ foldr (&&) True . sequA . fromMaybe 0 $ s'
    print $ bolt . fromMaybe 0 $ s'
    