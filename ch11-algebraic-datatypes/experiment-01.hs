module Data_vs_Value where

data Doggies a = Husky a 
               | Mastiff a
               deriving (Eq, Show) 

{-- What it means?
  [In GHCi]
  `:k Doggies String`  returns `Doggies String :: *`
  `:t Husky 10`        returns `Husky 10 :: Num a => Doggies a`
  `:t Husky (10::Int)` returns `Husky (10::Int) :: Doggies Int` 
--}

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = VietnamAirline 
             | CoronaNoFly 
             | AchooByeBye 
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price 
             | Plane Airline Size
             deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> Bool
areCars = and . (map (isCar))

manu :: Vehicle -> Manufacturer
manu (Car x _) = x
manu _         = error "Expected: Car \n Actual: Plane"

size :: Vehicle -> Integer
size (Plane _ (Size x)) = x
size _                  = error "Expected: Car \n Actual: Plane"

-- product type
data Owner = 
  Owner { name :: String, 
          age :: Int }
        deriving (Eq, Show)

thisOwner = Owner "Thomas" 21

{-- `name thisOwner` expects "Thomas"
    `age thisOwner` expects 21 
--}