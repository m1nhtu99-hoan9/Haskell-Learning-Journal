# Reading Notes on Chapter 10

```Haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

- `foldr` used in cases where recursive pattern associcating to the right are recognised
- Folding happens in two stages: _traversal_ and _folding_ (evaluation/reduction of folding function applied to values)
- Because of two-stage process & non-strict evaluation:
  - First bit of spine must be evaluated by `foldr`/`foldl` (see code demo in [fold-expmt-01.hs](./fold-expmt-01.hs))
  - If second argument not passed to folding functions, no more spines can be forced
  - Traversing the rest of spine may not happen if folding function doesn't ask for the rest of spine being evaluated (for instance, `const`)
