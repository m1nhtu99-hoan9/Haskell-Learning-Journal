{-# LANGUAGE InstanceSigs #-}

module MonadTrans where

import EitherT 
import ReaderT
import StateT 

class MonadTrans t where
    -- | Lift a computation from the argument monad to the constructed monad
    lift :: Monad m => m a -> t m a

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