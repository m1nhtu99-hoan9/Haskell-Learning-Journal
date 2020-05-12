import Data.Monoid

--MONAD LIST
{-
  (>>=) :: [a] -> (a -> [b]) -> [b]
-} 

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do 
  x <- xs -- xs@([x]) >>= \x -> if even x ...
  -- the binding performed there is somewhat like a list comprehension
  if even x 
    then [x*x, x*x]
    else [] -- odd values evaporated into the void {just kidding @@}

--MONAD MAYBE 
{-
  (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
  return ::       a ->  Maybe a 
-}

-- before we moove on

data Cow = Cow {
    name :: String
  , age  :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n 
  | n >= 0    = Just n
  | otherwise = Nothing

-- if Cow is older than 3 years old, its weigh must be above 50
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c 
      a = age c
  in if a < 3 && w < 50
     then Just c
     else 
        if a >= 3 && w >= 50
        then Just c
        else Nothing
         
mkCow :: String -> Int -> Int -> Maybe Cow
mkCow n a w = do
  name <- noEmpty n
  age <- noNegative a
  weight <- noNegative w
  weightCheck (Cow name age weight)

mkCowVerbose :: String -> Int -> Int -> Maybe Cow
mkCowVerbose n a w = 
  noEmpty n >>= 
  \name -> 
    noNegative a >>=
    \age -> 
      noNegative w >>=
      \weight -> 
      weightCheck (Cow name age weight) 

--MONAD EITHER

{-
  (>>=) :: Either a b -> (b -> Either a c) -> Either a c
 return ::          a -> Either e a
-}

data SoftwareShop = Shop {
    founded :: Int
  , programmers :: Int 
} deriving (Show, Eq)

data FoundedError = NegativeYears Int 
                  | TooManyYears Int
                  | NegativeCoders Int
                  | TooManyCoders Int
                  | TooManyCodersForYears Int Int
                  deriving (Show, Eq)
type EitherErrorInt = Either FoundedError Int

validateFounded :: Int -> EitherErrorInt
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 300   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> EitherErrorInt
validateCoders n 
  | n < 0     = Left $ NegativeCoders n
  | n > 400   = Left $ TooManyCoders n
  | otherwise = Right n

mkShop :: Int -> Int -> Either FoundedError SoftwareShop 
mkShop y c = do
  founded     <- validateFounded y
  programmers <- validateCoders c
  if programmers > (div founded 10)
    then Left $ (TooManyCodersForYears founded programmers)
    else Right $ Shop founded programmers

--SHORT EXERCISE
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

{- Test: Identity law: (==) (pure id <*> First' "Hello") (First "Helllo")
                       (==) (pure id <*> Second' "Hello") (Second' "Helllo")
         Homomorphism: 
            (==) (pure length <*> pure "Hello" :: Sum' String Int) (Second' 5)
         Interchange:  
            (==) ((Second' length) <*> pure "Hello") 
                 (pure ($ "Hello") <*> Second' length)
         Composition:
            (==) (pure (.) <*> Second' (+1) <*> Second' (*5) <*> Second' 4)
                 (Second' (+1) <*> (Second' (*5) <*> Second' 4)))

            
-}