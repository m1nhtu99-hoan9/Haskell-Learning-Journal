# Chapter 20: Foldables

## Reading Notes

- _Foldable_ is a way of _generalising catamorphisms_ to different datatypes. Foldable gives us a way to process values embedded
  in a structure as if they existed in a sequential order.

- `Foldale` intsance needs the respective `Monoid` instance.

```Haskell
class Foldable (t :: * -> *) where
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
[...]
    {-# MINIMAL foldMap | foldr #-}
```

- To have a working instance of `Foldable`, only either its `foldMap` or `foldr` need to be defined, [other operations](#derived-operations) can be derived from that.

### `foldMap`

- Examples of using `foldMap` to reduce to desired `Monoid` value:

```Haskell
Prelude> let fm = foldMap (*5)
Prelude> fm (Just 100) :: Product Integer
Product {getProduct = 500}
Prelude> fm (Just 5) :: Sum Integer
Sum {getSum = 25}
Prelude> fm Nothing :: Sum Integer
Sum {getSum = 0}
Prelude> fm Nothing :: Product Integer
Product {getProduct = 1}
```

### Other Basic Derived Operations <span id="derived-operations"></span>

```Haskell
class Foldable (t :: * -> *) where
  [...]
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
    {-# MINIMAL foldMap | foldr #-}
```

- Also:

```Haskell
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
  	-- Defined in ‘Data.Foldable’

```

## Recorded Errors & Misunderstanding While Doing Exercises

- In [`foldables-in-use.hs`](./foldables-in-use.hs):

  - I attempted:

  ```Haskell
  instance Monoid a => Foldable (Optional a) where [...]
  -- expected kind * -> *, but actually *
  ```
