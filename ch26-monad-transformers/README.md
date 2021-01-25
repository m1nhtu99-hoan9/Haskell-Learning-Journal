# Chapter 26: Monad Transformers

## Reading Notes

### `Writer`

- `Reader` lets us talk about values we need; `Writer` lets us deal with values we can omit and combine; `State` lets us both read and write values in any manner we desire.
- `transformers` library combines `Reader`, `Writer` and `State` into one big type (it's good to know something like that exists): 

```hs
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }
```

- Why not `Writer`?
  - `Writer` may be either too lazy or too strict for the problem at hand.
  - `Writer` can accumulate unevaluated thunks, causing memory leaks.

### Recovering an Ordinary Type from a Transformer

- Using `Identity`. For examples:

```hs
type MyIdentity a = IdentityT Identity a
type Maybe a = MaybeT Identity a
type Either e a = EitherT e Identity a
type Reader r a = ReaderT e Identity a
type State s a = StateT s Identity a
```

### Lexically Inner is Structurally Outer

- When Haskellers say base monad, they usually mean what is structurally outermost

### `MonadTrans` typeclass

```hs
class MonadTrans t where
    lift :: (Monad m) => m a -> t m a
```

## Related Learning Resources

- [StackOverflow explanation](https://stackoverflow.com/questions/18673525/is-it-possible-to-implement-applicative-m-applicative-statet-s-m) on why `Applicative (ReaderT s m)` requires a `m` constraint of `Monad m`

## Recorded Errors & Misconceptions During Doing Exercises

- `EitherT` exercise:

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

- "Wrap It Up" exercise:

```hs
type Embedded = 
    MaybeT 
      (ExceptT 
          String 
          (ReaderT () IO))
      Int

embedded :: Embedded
embedded = MaybeT . ExceptT . ReaderT $ (const . Right . Just $ 9) 

{-
    Expected type: () -> Either a0 (Either String (Maybe Int))
    Actual type: () -> Either a0 (Maybe Integer)

    -- hence, `a0` need to be properly lifted 
-}
```