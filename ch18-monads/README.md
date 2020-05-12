# Chapter 18: Monads

## Reading Notes

- _Monad_ is _applicative functor_.
  - An applicative maps a function that is contained in some structure over some structure an then combine the 2 layers of structure.
- In Haskell, `Applicative` is superclass of `Monad`. Chain of dependency: `Functor -> Applicative -> Monad`.

### Core Operations

```Haskell
(>>=)  :: m a -> (a -> m b) -> m b
(>>)   :: m a ->    m b     -> m b
return ::   a ->    m a
```

- `return` behave identically to Applicative's `pure`
- `>>` (sequencing operator) sequence two actions while discarding any result of the first action
- `>>=` (bind) is what we use when we colloquially say that we use monads.

### About Bind

```Haskell
fmap :: Functor f     => (a -> b)   -> f a        -> f b
<*>  :: Applicative f => f (a -> b) -> f a        -> f b
>>=  :: Monad f       => f a        -> (a -> f b) -> f b
-- kinda look like the flipped version of `fmap` and `<*>`
```

- Because Functor `fmap` and Applicative `<*>` preserve the structure, here is what happen when fmap apply over the arguments of `>>=`:

```Haskell
fmap :: Monad m => m a -> (a -> m b) -> m (m b)
```

- It means that while `fmap` and `<*>` keep the structure intact, `>>=` can inject/flatten layers of structure through function application.

### Misconceptions about Monads

A monad **is not**:

1. **impure**. Monadic functions are pure functions. `IO` is abstract datatype that allows for impure & effectful actions. `IO` has `Monad` instance.
2. **embedded language for imperative programing**. While monads are used for sequencing actions, _commutative monads_ do not order the actions.
   > "Haskell is the world's finest imperative programming language" - Simon Peyton-Jones
3. **a value**. It kinda like an ADT.
4. **about strictness**. The monadic operations of `bind` and `return` are nonstrict. However, some operations can be made strict with specific instance.

### Do Syntax and Monads

```Haskell
-- getLine :: IO String ;  putStrLn :: String -> IO ()

bindGetAndPut :: IO ()
bindGetAndPut = do
  theString <- getLine
  putStrLn theString

-- the above one behave identically to the below one:

bindGetAndPut' :: IO ()
bindGetAndPut' = getLine >>= putStrLn
-- bindGetAndPut' :: IO String >>= (String -> IO ()) -> IO ()
```

- What does `do` do?

```Haskell
do
  x <- a; b
-- is desugared to `a >>= \x -> b` (perform a, bind its result to x, perform b with x in scope)
```

### Monads In Use

- Monad List
- Monad Maybe vs Applicative Maybe:
  - With `Maybe Applicative`, each computation fails or succeed independently of each other (kinda analogous to components of parallel electrical circuit).
  - With `Maybe Monad`, computations contributing to the final result can choose to return `Nothing` based on previous computation (kinda like a chain of JS Promises or components of series electrical circuit).
- _Notes_:

  - Applicative and Monad instances must have the same behaviour:

  ```Haskell
  import Control.Monad (ap)

  (<*>) == ap
  ```

### Monad Laws

1. Identity law

- Right Identity

```Haskell
m >>= return = m
```

- Left Identity

```Haskell
return x >>= f = f x
```

- These two Identity laws signify that `return` should change any behaviours, just "lift" the structure (similarly to `pure`).

2. Associativity

```Haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

### Composition for Monadic Functions

- Kleisli composition: kinda like a pipeline. In fact, it's exactly identical to `flip (.)`:

```Haskell
import Control.Monad

(>=>)
  :: Monad m
  => (a -> m b) -> (b -> m c) -> a -> m c

flip (.) -- exactly pipeline
  :: (a -> b) -> (b -> c) -> a -> c
```

## Recorded Errors & Misunderstanding While Doing Exercises

- Finally I understood how typeclass `Arbitrary` of `QuickCheck` works.
- In [`monads-in-use.hs`](./monads-in-use.hs):

  - I attempted:

  ```Haskell
  instance (Monoid a) => Applicative (Sum' a) where
  -- [...]
  (First' x1)  <*> (First' y1)  = First' (x1 <> y1)
  -- [...]
  trgSum :: Sum' [Int] (Maybe String, String, [Int])
  trgSum = undefined
  ```

  The constraint I defined here doesn't account for all the cases, which came from the misconception that Applicative instance has to abide by Monoid laws. For all cases of `First' x1 <*> _`, it can't apply itself to anything, hence it only need to abide by Identity law.

  When I tested that Applicative instance with `checkers`:

  ```Haskell
  applicative:
    identity:     +++ OK, passed 500 tests.
    composition:  +++ OK, passed 500 tests.
    homomorphism: +++ OK, passed 500 tests.
    interchange:  +++ OK, passed 500 tests.
    functor:      +++ OK, passed 500 tests.

  monad laws:
    left  identity: +++ OK, passed 500 tests.
    right identity: +++ OK, passed 500 tests.
    associativity:  +++ OK, passed 500 tests.
    pure:           +++ OK, passed 500 tests.
    ap:             *** Failed! Falsifiable (after 2 tests):
  First' []
  First' [-1]
  ```

- In chapter exercises:

  - My 1st attempt to write `Applicative` and `Monad` instances for `List`:

  ```Haskell
  instance Applicative List where
    pure x0                  = Cons x0 Nil
    Cons f Nil <*> Cons x ls = Cons (f x) (fmap f ls)
    _          <*> Nil       = Nil
    Nil        <*> _         = Nil

  instance Monad List where
    return          = pure
    Nil       >>= f = Nil
    Cons x ls >>= f = Cons (f x) (ls >>= f)
  ```

  - My 2nd attempt to write `Applicative` and `Monad` instance for `List`:

  ```Haskell
  instance Applicative List where
    pure x0           = Cons x0 Nil
    fs <*> ls         = listToRecurList [
                         f x
                       | f <- recurListToList fs
                       , x <- recurListToList ls
                      ]
  instance Monad List where
    return          = pure
    ls >>= fs       = listToRecurList [
                        xs
                      | f  <- recurListToList fs
                      , xs <- fmap f $ recurListToList ls
                    ]
  ```
