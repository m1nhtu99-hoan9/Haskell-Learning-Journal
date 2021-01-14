{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Typeable

data Reader r a = 
  Reader { 
    runReader :: r -> a 
  } deriving (Typeable)
  

{- READING COMPREHENSION -}
myLiftA2 :: Applicative f => (a -> b -> c) 
                              -> f a -> f b -> f c
-- copied from `Prelude`'s source code of `liftA2`
-- this generalised implementation may not be performance-wise for some Applicative instances 
myLiftA2 f x y = f <$> x <*> y 

ask :: (r -> a) -> Reader r a
ask = Reader

instance Functor (Reader r) where
  -- fmap :: a -> Reader _d -> _c
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  -- `(<*>) = _a` so that GHC will suggest type signature for hole `_a`
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  -- rF'' <*> rF = Reader (\x -> (runReader rF'' r) (runReader rF x))
  (Reader f'') <*> (Reader f) = Reader (\x -> f'' x (f x))
  -- "lift" binary functions 
  pure :: a -> Reader r a
  pure x = Reader (\_ -> x)

