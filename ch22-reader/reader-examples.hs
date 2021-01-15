{-# LANGUAGE InstanceSigs #-}
import Control.Applicative

{- DEMO WHAT PROBLEMS COULD BE SOLVED BY READER APPLICATIVE & MONAD -}
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

-- w/ Reader Monad
getDogOf' :: Person -> Dog
getDogOf' = do
  d <- dogName
  a <- address
  return (Dog d a)
