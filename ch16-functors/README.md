# Chapter 16: Functors

## Reading Notes

- Trivia: the word _functor_ was borrowed by mathematicians from the philosopher Rudolf Carnap, who invented this word during 1930s in linguistic context.
- _Functor_ is a way to apply a function over/around a structure without altering the structure.
- Similar to _Monoid_, _Functor_ is implemented in Haskell as a typeclass.

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
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
fmap2 . fmap1 :: ((g x -> g y) -> (f (g x) -> f (g y))) -> ((x -> y) -> (g x -> g y)) -> ((x -> y) -> (f (g x) -> f (g y)))

Hence, compose fmap1 to fmap2, we have the end result having type of (x -> y) -> (f (g x) -> f (g y))
```

- Example:

```Haskell
(fmap . fmap) (const 'p') [Just "Dance", Nothing, Just "Fly"]
-- the first fmap result to a type of `Char -> String`
-- the second fmap apply `Char -> String` to `[Maybe String]`, hence the end result having type `[Maybe String]` -> [Maybe Char]`
```

## Recorded Errors & Misunderstanding While Doing Exercises
