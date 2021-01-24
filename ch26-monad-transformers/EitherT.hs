{-# LANGUAGE InstanceSigs #-}

module EitherT where

import Control.Applicative ( liftA2 )

newtype EitherT e m a = 
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure = EitherT . pure . pure

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT mef) <*> (EitherT mea) = EitherT $ liftA2 (<*>) mef mea

instance Monad m => Monad (EitherT e m) where
    return = pure 

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT ema) >>= f = EitherT $ do
        x0 <- ema -- ema :: m (Either e a); x0 :: Either e a
        case x0 of 
            Right x -> runEitherT $ f x
            Left y -> return (Left y) -- the `Left` context need to be specified

{- HELPER FUNCTIONS -}

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema
  where
    swapEither :: Either a e -> Either e a
    swapEither (Left x) = Right x
    swapEither (Right x) = Left x

eitherT :: Monad m 
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mab) = do
    x <- mab
    case x of
        Left xa -> f xa
        Right xb -> g xb

-- test data

x0 :: EitherT String Maybe Int
x0 = pure 8

x1 :: EitherT String Maybe Int
x1 = EitherT (Just . Left $ "Nothing to be scared for")

xf0 :: EitherT String Maybe (Int -> String)
xf0 = pure (concat . flip replicate "uchihahaha")

main :: IO ()
main = do
    let printEitherT mm = print $ runEitherT mm
    printEitherT $ (*2) <$> x0
    printEitherT $ (*2) <$> x1
    printEitherT $ xf0 <*> x0
    printEitherT $ xf0 <*> x1
    printEitherT $ swapEitherT x1

