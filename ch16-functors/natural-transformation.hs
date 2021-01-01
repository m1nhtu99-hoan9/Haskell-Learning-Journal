{-# LANGUAGE RankNTypes #-}
-- in GHCi: :set -XRankNTypes

-- transform the structure, perserve the values 
type Nat f g = forall a . f a -> g a

-- structural transformation is not a fold
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) =  [x]