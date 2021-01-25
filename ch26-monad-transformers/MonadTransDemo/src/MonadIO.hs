{-# LANGUAGE InstanceSigs #-}

module MonadIO where

import MaybeT
import ReaderT
import StateT

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

-- | 1
-- liftIO . return = return
-- | 2
-- liftIO (m >>= f) = liftIO m >>= (liftIO . f)

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO :: IO a -> (MaybeT m a)
    liftIO = lift . liftIO 
      where 
        lift = MaybeT . fmap Just

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO :: IO a -> (ReaderT r m a)
    liftIO = lift . liftIO
      where 
        lift = ReaderT . const

instance MonadIO m => MonadIO (StateT s m) where
    liftIO :: IO a -> (StateT s m a)
    liftIO = lift . liftIO 
      where
        lift :: Monad m => m a -> StateT s m a
        -- m a -> StateT (s -> m (a, s))
        lift ma = 
            -- kinda similar to `Control.Monad.Trans.State.Strict.get`
            StateT $ \x0 -> do
                a0 <- ma
                return (a0, x0)

