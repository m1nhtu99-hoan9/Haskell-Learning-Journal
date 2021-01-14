{-# LANGUAGE InstanceSigs #-}
import Control.Applicative

-- `runReader` is an "accessor"
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  -- fmap f (Reader g) = Reader $ \r -> f (g r)
  fmap f (Reader g) = Reader (f . g)

-- `fmap` for `Functor (Reader r)` is the same as dot operator
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = f . g

{- the most generic implementation for `ask` -}
ask :: Reader a a
ask = Reader id

{- DEMO WHAT PROBLEMS COULD BE SOLVED BY READER APPLICATIVE -}
newtype HumanName = HumanName String 
                    deriving (Eq, Show)

newtype DogName = DogName String
                  deriving (Eq, Show)

newtype Address = Address String 
                  deriving (Eq, Show)

data Person = 
    Person {
      humanName :: HumanName
	, dogName :: DogName
	, address :: Address
    } deriving (Eq, Show)                  

data Dog =
	Dog {
	  dogName' :: DogName
	, dogAddress :: Address
	} deriving (Eq, Show)

p1 :: Person
p1 = Person 
	(HumanName "Thomas")
	(DogName "Nasus")
	(Address "Sesame Street")

getDogOf :: Person -> Dog
getDogOf = liftA2 Dog dogName address 