# Chapter 26: Monad Transformers

## Reading Notes

## Recorded Errors & Misconceptions During Doing Exercises

```haskell
instance Applicative m => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    -- which make `pure`-ify of Left value hard to define
    pure = EitherT . pure . Right

instance Monad m => Monad (EitherT e m) where
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT ema) >>= f = do
        x0 <- ema -- ema :: m (Either e a); x0 :: Either e a
        case x0 of 
            Right x -> f x
            l@(_) -> EitherT $ return l

{-
• Occurs check: cannot construct the infinite type: m ~ EitherT e m
      Expected type: EitherT e m (Either e a)
      Actual type: m (Either e a)
-}
```