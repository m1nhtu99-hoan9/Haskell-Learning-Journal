data FlowerType = Gardenia
                | Daisy
                | Rose 
                | Lilac
                deriving Show

type Gardener = String 

data Garden = 
  Garden Gardener FlowerType
  deriving Show

{-- `Farmhouse` & `Farmhouse'` are semantically the same
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
