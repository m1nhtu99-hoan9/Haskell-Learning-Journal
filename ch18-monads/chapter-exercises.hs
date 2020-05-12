{-
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}
instance Applicative List where 
  pure x0           = Cons x0 Nil
  fs <*> ls         = undefined

{-
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
-}
instance Monad List where
  return          = pure
  ls >>= fs       = undefined 
