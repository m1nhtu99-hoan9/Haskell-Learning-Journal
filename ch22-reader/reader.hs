data R r a = R (r -> a)

runR :: R r a -> r -> a
runR (R f) e = f e

instance Functor (R r) where
  -- fmap :: (a -> b) -> R r a -> R r b
  fmap f (R ra) = R $ \r -> f (ra r)

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)