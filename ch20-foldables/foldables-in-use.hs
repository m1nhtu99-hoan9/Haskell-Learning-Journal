import Data.Monoid
import Data.Foldable

data Identity a = Identity a 
                deriving (Show, Eq)

instance Foldable Identity where 
  -- `f` is binary operation
  foldr f i (Identity x) = f x i -- right associativity
  foldl f i (Identity x) = f i x -- left associativity
  foldMap f (Identity x) = f x

{-
*Main> :t foldMap (*5) (Identity 5) 
foldMap (*5) (Identity 5) :: (Num m, Monoid m) => m
-}

data Optional a = Nada 
                | Yep a
                deriving (Show, Eq)

instance Foldable Optional where
  foldr _ i Nada    = i
  foldr f i (Yep x) = f x i

  foldl _ i Nada    = i
  foldl f i (Yep x) = f i x

  foldMap _ Nada    = mempty --just evaluate to the identity value 
  foldMap f (Yep x) = f x            