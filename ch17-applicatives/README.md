# Chapter 17: Applicatives

## Reading Notes

- Monoidal Functors. `Applicative` typeclass allows for function application lifted over structure (like `Functor`).

```Haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

What it means is that any `Applicative` also has a `Functor`. Vice versa, `Functor` can also be defined in term of `Applicative`.

- `pure` lifts something into functorial (_applicative_) structure.
- `<*>` is called _apply_ or shortened into _ap_.

### Applicative vs Functor

```Haskell
fmap f x = pure f <*> x

Prelude> fmap (+1) [1, 2, 45] --fmap (Int -> Int) [Int]
[2,3,46]
Prelude> pure (+1) <*> [1, 2, 45] --pure (Int -> Int) <*> [Int]
[2,3,46]

-- `pure` lifts (Int -> Int) into ([Int] -> [Int])
```

### Why is Applicative Monoidal Functor?

```Haskell
($)   ::                    (a -> b) ->  a  ->  b
(<$>) :: Functor f =>       (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- `f` in the definition of `<*>` represents functorial structure
--     that is on the outside of the function.
```

- `$` acts as a proxy for ordinary function application; `<$>` lift `(a -> b)` function over the instance of `f` _wrapped_ around value `a`.

```Haskell
mappend ::       f        f      f
$       ::   (a -> b) ->  a  ->  b
<*>     :: f (a -> b) -> f a -> f b
```

- Note that a type can have multiple `Monoid` instances but can have only one `Functor` instance.

## Recorded Errors & Misunderstanding While Doing Exercises
