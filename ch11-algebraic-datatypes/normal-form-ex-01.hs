data Sum a b = First a | Second b deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)

data FlowerType = Gardenia
                | Daisy
                | Rose 
                | Lilac
                deriving Show

type Gardener = String 

data Garden = 
  Garden Gardener FlowerType
  deriving Show

{-- @region: Demonstration on defining product types:
    `Farmhouse` & `Farmhouse'` are semantically the same, 
    `ExtendedFarmhouse` & `ExtendedFarmhouse'` are semantically the same
--}

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
newtype NumSheep = NumSheep Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
data ExtendedFarmhouse = 
  ExtendedFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig
type ExtendedFarmhouse' = Product NumCow (Product NumPig NumCow)

-- @endregion

{-- @region: Demonstration on defining sum type:
    `Animal` & `Animal'` are semantically the same, 
--}

type Name = String --type alias/synonym
type Age = Int 
type LovesMud = Bool
type KilosOfWool = Float

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age KilosOfWool deriving (Eq, Show)

data Animal = Cow CowInfo |
              Pig PigInfo |
              Sheep SheepInfo
              deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- @endregion