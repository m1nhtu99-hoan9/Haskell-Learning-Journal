# Chapter 16: Functors

## Reading Notes

- Trivia: the word _functor_ was borrowed by mathematicians from the philosopher Rudolf Carnap, who invented this word during 1930s in linguistic context.
- _Functor_ is a way to apply a function over/around a structure without altering the structure.
- Similar to _Monoid_, _Functor_ is implemented in Haskell as a typeclass.

```Haskell
*Main> :i Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
  	-- Defined in ‘GHC.Base’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Functor ((->) r) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
```

- `<$>` is infix version of `fmap`:

```Haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
($) :: (a -> b) -> a -> b
```

- Review: Type construtors ~ Higher-kinded types

### Functor Laws

Functors must be structure preserving by abiding these 2 laws:

1. **Indentity**: `fmap id == id`
2. **Composition**: `fmap (f ∘ g) == fmap f ∘ fmap g`

### Functors Can Be Stacked

- When multiple layers of functorial structure are recognised, `fmap`'s can be "stacked".

- Proof:

```Haskell
-- given that:
(.) :: (a -> b) -> (r -> a) -> (r -> b)
fmap1 :: (x -> y) -> g x -> g y
fmap2 :: (u -> v) -> f u -> f v

-- to unify/leverage types
(.) :: (a -> b) -> (r -> a) -> (r -> b)
fmap1 :: (x -> y) -> g x -> g y
fmap2 :: (g x -> g y) -> f (g x) -> f (g y)

-- a ::= (g x -> g y) {has kind * -> *}; b ::= f (g x) -> f (g y);
-- r ::= (x -> y)
fmap2 . fmap1 :: ((g x -> g y) -> (f (g x) -> f (g y))) -> ((x -> y) -> (g x -> g y))
                  -> ((x -> y) -> (f (g x) -> f (g y)))

-- Hence, compose fmap1 to fmap2, we have the end result having type of (x -> y) -> (f (g x) -> f (g y))
```

- Example:

```Haskell
(fmap . fmap) (const 'p') [Just "Dance", Nothing, Just "Fly"]
-- the first fmap result to a type of `Char -> String`
-- the second fmap apply `Char -> String` to `[Maybe String]`
-- , hence the end result having type `[Maybe String]` -> [Maybe Char]`
```

### Eta-reduced `fmap`

```Haskell
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show
```

### Structural Transformation

- Structural transformation preserving the structure's values is possible. This technique is called "_natural transformation_"
- To do this, `RankNTypes` option need to be activated:
  - In GHCi: `:set -XRankNTypes`
  - In code file: put on the first code line, `{-# LANGUAGE RankNTypes #-}`
- Example:

```Haskell
type Nat f g = forall a . f a -> g a

-- structural transformation is not a fold
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) =  [x]
```

### Functors are unique to a datatype

- In Haskell, `Functor` instances are unique for their given datatypes. In constrast, for `Monoid`, these unique pairings requires the use of `newtype`s to pair `Monoid` instance to a type.

## Recorded Errors & Misunderstanding While Doing Exercises

- ~~I haven't been able to extract value inside `Flip` `newtype` value instances yet.~~ I've done it in [`chapter-exercises.hs`](./chapter-exercises.hs).
