# Reading Notes on Chapter 10

```Haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

## `foldr`

[![foldr structured transformation](./.__img/Right-fold-transformation.png)](https://wiki.haskell.org/Fold)

- `foldr` used in cases where recursive pattern associcating to the right are recognised
- Folding happens in two stages: _traversal_ and _folding_ (evaluation/reduction of folding function applied to values)
- Because of two-stage process & non-strict evaluation:
  - First bit of spine must be evaluated by `foldr`/`foldl` (see code demo in [fold-expmt-01.hs](./fold-expmt-01.hs))
  - If second argument not passed to folding functions, no more spines can be forced
  - Traversing the rest of spine may not happen if folding function doesn't ask for the rest of spine being evaluated (for instance, `const`)

## `foldl`

[![foldl structured transformation](./.__img/Left-fold-transformation.png)](https://wiki.haskell.org/Fold)

- The result of `foldl` is accumulated to the first argument, not the list argument (see code demo in [reimplement-standard-functions.hs](./reimplement-standard-functions))
- `foldl` has successive steps of folding as first argument, whereas `foldr` has successive recursions intermediated by the folding function (see code demo in [reimplement-standard-functions.hs](./reimplement-standard-functions)). Hence, recursion of spine in `foldl` is unconditionally unsafe (for demonstration, see `example9` to `example12` in [fold-expmt-01.hs](./fold-expmt-01.hs)).
- For safe `foldl` perfomance, use `Data.List.foldl'`

```Haskell
last (scanl f z xs) = foldl f z xs
head (scanr f z xs) = foldr f z xs
```
