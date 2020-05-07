# Chapter 15: Monoids & Semigroups

## Reading Notes

- Algebra-as-typeclass:

```Haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

- `mempty` is identity value.
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

### _Commutativity_ vs _Associativity_

- _Commutativity_ is stronger than _Associativity_. For instance, addition and multiplication for the number type are commutative, but `(++)` for the list type is associative.
  - Distributed systems use commutative monoids in designing and thinking about constraints, which are monoids that guarantee their operation commutes.

### `Integer` Doesn't Have `Monoid` Instance

_Integer_ forms a _Monoid_ under **summation** and **multiplication**. Unique instance rules for these two monoid
are enforced by using `Data.Monoid.Sum` & `Data.Monoid.Product`.

Generally, the unique instance rule of is enforced by using `newtype` to separate the different monoidal behaviors.

### `QuickCheck`

- Validate associativity:

```Haskell
\ a b c -> (==) (a + b + c) (a + (b + c))
\ (<>) a b c -> a <> (b <> c) == (a <> b) <> c
\ f a b c -> (==) (f (a + b) + f c) (f a + f (b + c))
```

### Semigroup

- _Semigroup_ is _Monoid_ without identity: `(a <> b) <> c = a <> (b <> c)`

```Haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

- _Semigroup_ is weaker algebra than _Monoid_.
- Example: `Data.List.NonEmpty`

## Recorded Errors & Misunderstanding While Doing Exercises

- In [`optional-monoid.hs`](./optional-monoid.hs):
  - Attempt to pattern match a `Monoid (Optional a)` with a `Monoid a`
  ```Haskell
  instance Monoid a => Monoid (Optional a) where
  mappend (Only x1) (Only x2) = mappend x1 x2
  ```
- In `semigroup-exercises.hs`:
  - Failed attempt to define `type IdenAssoc = (Eq t, Semigroup t) => Identity t -> Identity t -> Identity t -> Bool`
  - Misunderstanding about the type definition of `boolConjGen` as I attempted: `boolConjGen :: Arbitrary Bool -> Gen (BoolConj Bool)` and `boolConjGen :: Arbitrary Bool -> Gen BoolConj`
