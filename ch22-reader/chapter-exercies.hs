{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Typeable

data Reader r a = 
  -- `runReader` is called "accessor"
  Reader { runReader :: r -> a } 
  deriving (Typeable)

{- READING COMPREHENSION exercies -}
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
  
  -- to "lift" binary function
  pure :: a -> Reader r a
  -- pure x = Reader (\_ -> x)
  pure x = Reader (const x)

{- READER MONAD exercises -}
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  rF >>= fRg = Reader (\r -> runReader (r0 r) $ r)
    where 
      -- step 0: given the first argument as a Reader, extract its unary function `f`
      f = runReader rF
      -- step 1: evaluate the input for the second argument `fRg`,  
      --         then apply it to `fRg` to extract the inner Reader
      --         explicitly: `r0 r = fRg (f r)` 
      r0 = fRg . f
      
      


