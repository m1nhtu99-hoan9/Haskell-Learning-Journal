-- eta-reduced & point-free version
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a = LolNope | Yepper a 
                  deriving (Show, Eq)
instance Functor Possibly where
  fmap f LolNope    = LolNope 
  fmap f (Yepper a) = Yepper (f a)
  
data SumPossibilities a b = First a | Second b 
                            deriving (Show, Eq)
instance Functor (SumPossibilities a) where -- * -> * -> b -> Second b
  fmap f (Second y) = Second (f y) 
  fmap _ (First x)  = First x