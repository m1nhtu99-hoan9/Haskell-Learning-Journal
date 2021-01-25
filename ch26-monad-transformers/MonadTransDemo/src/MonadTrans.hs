{-# LANGUAGE InstanceSigs #-}

module MonadTrans where

import Control.Monad.Trans.Class

import EitherT 
import ReaderT
import StateT 

{- EXERCISES -}

instance MonadTrans (EitherT e) where
    lift :: Monad m => m a -> EitherT e m a
    lift ma = EitherT $ pure <$> ma

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    -- m a -> StateT (s -> m (a, s))
    lift ma = 
        -- kinda similar to `Control.Monad.Trans.State.Strict.get`
        StateT $ \x0 -> do
            a0 <- ma
            return (a0, x0)
