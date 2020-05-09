data Wrap f a = Wrap (f a) deriving (Show, Eq)
-- `f` is a function definition; `a` is a constant value 

instance (Functor f) => Functor (Wrap f) where
  fmap f' (Wrap fa) = Wrap (fmap f' fa)

-- beside the sake of experimenting, this technique has no real practical purpose
-- in cases this technique may be needed, composition of `fmap`'s suffices. 