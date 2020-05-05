# Chapter 15: Monoids & Semigroups

```Haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

- `<>` is infix version of `mappend`.

- _List_ forms a _Monoid_ under concatenation. List also have more possible monoid.

## `Integer` doesn't have `Monoid` instance

_Integer_ forms a _Monoid_ under **summation** and **multiplication**. Unique instance rules for these two monoid
are enforced by using `Data.Monoid.Sum` & `Data.Monoid.Product`.

Generally, the unique instance rule of is enforced by using `newtype` to separate the different monoidal behaviors.
