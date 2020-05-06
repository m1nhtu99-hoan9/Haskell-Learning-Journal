# Chapter 15: Monoids & Semigroups

- Algebra-as-typeclass:

```Haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

- `<>` is infix version of `mappend`.

- Mappending is less about _combining_ but about _condensing_ or _reducing_.

  - For instance, `Maybe` type:

  ```Haskell
  Prelude Data.Monoid> First (Just 'a') <> First (Just '!')
  First {getFirst = Just 'a'}
  Prelude Data.Monoid> Last (Just 'a') <> Last (Just '!')
  Last {getLast = Just '!'}
  ```

- _List_ forms a _Monoid_ under concatenation. List also has more possible monoids.

## `Integer` Doesn't Have `Monoid` Instance

_Integer_ forms a _Monoid_ under **summation** and **multiplication**. Unique instance rules for these two monoid
are enforced by using `Data.Monoid.Sum` & `Data.Monoid.Product`.

Generally, the unique instance rule of is enforced by using `newtype` to separate the different monoidal behaviors.

## Recorded Errors & Misunderstanding While Doing Exercises

- In [`optional-monoid.hs`](./optional-monoid.hs):
  - Attempt to pattern match a `Monoid (Optional a)` with a `Monoid a`
  ```Haskell
  instance Monoid a => Monoid (Optional a) where
  mappend (Only x1) (Only x2) = mappend x1 x2
  ```
