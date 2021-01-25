{-# LANGUAGE InstanceSigs #-}

module ReaderT where

import Control.Applicative ( liftA2 )

newtype ReaderT r m a = 
    ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    -- apply f to 2 functorial layers of `Reader` and `m`
    fmap f (ReaderT fRma) = ReaderT $ (fmap . fmap) f fRma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure x = ReaderT $ pure . pure $ x

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    -- `(<*>)` need to be lifted over the applicative layer of `m`
    (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ liftA2 (<*>) rmf rma

instance (Monad m) => Monad (ReaderT r m) where
    return = pure

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= fRmb = ReaderT $ \r0 -> do
        rmb <- rma r0 
        --  ^ to retrieve `r -> m a` out of `ReaderT` data constructor and apply it to `r0`
        --    `<-` bind the `a` out of monadic structure of `m a`
        runReaderT (fRmb rmb) r0
        --  ^ `fRmb rmb` unpack the `r -> m b`