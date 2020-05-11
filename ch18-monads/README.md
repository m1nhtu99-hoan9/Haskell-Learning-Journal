# Chapter 17: Monads

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

## Recorded Errors & Misunderstanding While Doing Exercises
