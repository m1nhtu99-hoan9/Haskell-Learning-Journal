{-# LANGUAGE InstanceSigs #-}

module StateT where

import Control.Applicative ( liftA2 )

newtype StateT s m a = 
    StateT { runStateT :: s -> m (a, s) }

{- STRICT IMPLEMENTATIONS -}

instance (Functor m) => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    -- rely on pattern matching to unwrap `fSma s0`
    fmap f (StateT fSma) = StateT $ \s0 -> (fT f) <$> (fSma s0)
      where
        fT :: (a -> b) -> (a, s) -> (b, s)
        fT f0 (x0, s1) = (f0 x0, s1) 

-- The expected order-dependent computation for the `StateT Applicative` to have
-- can't be expressed with having the `Monad` constraint for `m`
-- [Reference](https://stackoverflow.com/questions/18673525/is-it-possible-to-implement-applicative-m-applicative-statet-s-m)
instance (Monad m) => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    -- inner `pure` lift the tuple to be inside a applicative/monadic structure
    pure x = StateT $ \s0 -> pure (x, s0)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT smf) <*> (StateT sma) = 
        -- these threading of computations below requires `m` to be Monad
        StateT $ \s0 -> do
            -- extract `f0` of type `a -> b` as the 1st inner `State`'s monadic computation
            (f0, s1) <- smf s0
            -- extract `x0` of type `a` as the 2nd `State`'s monadic computation
            (x0, s2) <- sma s1
            -- `s2` is the final state; `f0 x0` is the desired inner `ap` evaluation
            return (f0 x0, s2)

instance (Monad m) => Monad (StateT s m) where
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= fSmb = StateT $ \s0 -> do
        (a0, s1) <- sma s0 
        --  ^ to retrieve `s -> m (a, s)` out of `StateT` data constructor and apply it to `r0`
        --    `<-` bind the `a` out of monadic structure of `m (a, s)` (with the help of pattern matching)
        runStateT (fSmb a0) s1
        --  ^ `fSmb smb` unpack the `s -> m (b, s)`