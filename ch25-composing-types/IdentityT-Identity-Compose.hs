{-# LANGUAGE InstanceSigs #-}

import Control.Applicative ( liftA2 )
import Data.Foldable
import Data.Traversable

{- Custom instances of Identity and Compose -}

newtype Identity a = 
    Identity { runIdentity :: a }
    deriving (Eq, Show) 

newtype IdentityT f a = 
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

newtype Compose f g a = 
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

{- Instances for `Identity` -}

instance Functor Identity where
    -- do nothing special
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where 
    return = pure
    (Identity a) >>= f = (f a)

{- Instances for `Compose` -}

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

    {- EXERCISE: "Twinlicative" -}
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose . pure . pure 

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    Compose fgh <*> Compose fga = Compose $ liftA2 (<*>) fgh fga

    {- EXERCISE: "Compose Instances" -}
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    -- let `foldMap` be applied on 2 foldable layers of structure
    foldMap f0 (Compose fga) = foldMap (foldMap f0) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative p => (a -> p b) -> Compose f g a -> p (Compose f g b)
    -- `traverse` applied twice return an applicative having 2 nested applicative layer of structures
    -- `Compose` need to be `fmap`ed over to get the final result
    traverse p0 (Compose fga) = Compose <$> traverse (traverse p0) fga

{- Instances for `IdentityT` -}

instance Functor m => Functor (IdentityT m) where
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f (IdentityT ga) = IdentityT (fmap f ga) -- `ga` inner structure is untouchable

instance Applicative m => Applicative (IdentityT m) where
    pure :: a -> IdentityT m a
    pure = IdentityT . pure

    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    (IdentityT fab) <*> (IdentityT ga) = IdentityT (fab <*> ga)

instance Monad m => Monad (IdentityT m) where 
    return = pure
    
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= tf0 = 
        -- unpack `ma` out of `IdentityT` context then bind it over
        -- the inner bind which has type signature of `a -> m b` 
        IdentityT (ma >>= runIdentityT . tf0)

