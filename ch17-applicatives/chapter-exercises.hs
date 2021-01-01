import Data.Monoid
import Control.Applicative (liftA3)

--EXERCISE "Write Instances"
--  question 1
data Pair a = Pair a a 
              deriving (Eq, Show)
instance Functor Pair where 
  fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
  pure x                        = Pair x x
  (Pair f1 f2) <*> (Pair y1 y2) = Pair (f1 y1) (f2 y2)

--  question 2
data Two a b = Two a b 
               deriving (Eq, Show)
instance Functor (Two a) where 
  fmap f (Two x y) = Two x (f y)
  -- only the second value is able to be `fmap`ed over 
instance (Monoid a) => Applicative (Two a) where
  pure x                      = Two mempty x
  (Two x1 x2) <*> (Two y1 y2) = Two (x1 <> y1) (x2 y2)
  -- because the first value having type `a` can't be `fmap` over,
  -- it only need to adhere to Monoid laws

--  question 3
data Three a b c = Three a b c 
                   deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x  y (f z)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure z                                = Three mempty mempty z
  (Three x1 x2 x3) <*> (Three y1 y2 y3) = Three (x1 <> y1) (x2 <> y2) (x3 y3)

--  question 6
data Four' a b = Four' a a b b
                 deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 x4) = Four' x1 x2 (f x3) (f x4)
instance (Monoid a) => Applicative (Four' a) where
  pure z                                      = Four' mempty mempty z z
  (Four' x1 x2 x3 x4) <*> (Four' y1 y2 y3 y4) =
    Four' (x1 <> y1) (x2 <> y2) (x3 y3) (x4 y4)

--EXERCISE "Combinations" 

stops :: String 
stops = "pbtdkg"

vowels :: String 
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos ss vs ss' = liftA3 (,,) ss vs ss'
--combos ss vs ss' = (,,) <$> ss <*> vs <*> ss'

-- not a smart way but acceptable in this case
ternaryTupleToList :: (a, a, a) -> [a]
ternaryTupleToList (x1, x2, x3) = [x1, x2, x3]