# Chapter 21: Traversables

## Reading Notes

- Traversable is described as as a way to traverse a data structure, mapping a function inside a structure while accumulating the applicative contexts along the way.

- In literal sense, anytime we need to flip two type constructors around, or map something and flip them around, that's probably `Traversable`.

- `Traversable` depends on `Applicative`, and thus `Functor`, and is also superclassed by `Foldable`.

```Haskell
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
```

- `traverse` maps each element of a structure to an action, evaluates the actions from left to right and collects the results.

```Haskell
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traverse f = sequenceA . fmap f
sequenceA :: Applicative f =>  t (f a) -> f (t a)
sequenceA = traverse id
```

- using `sequenceA` flips the structure around.

- While `Foldable` seems trivial, it is a necessary superclass of `Traversable`, and `Traversable`, like `Functor` and `Monad`, is now widely used in everyday Haskell code, due to its practicality.

### `traverse`

```Haskell
fmap     :: (a -> b)   -> f a -> f b
(=<<)    :: (a -> m b) -> m a -> m b
traverse :: (a -> f b) -> t a -> f (t b)
```

- `traverse` still map a function over embedded values, but the function itself generate more structure, similarly to flip bind. Unlike `fmap` & `(=<<)`, the structure can be of a different type than the structure being lifted over. At the end, 2 structures embedded in the value will be flipped around, as `sequenceA` behaves.

- It's better to use `traverse` whenever we see a `sequence` or `sequenceA` combined with a `map` or `fmap`.

### Strength of Traversables

- `Functor` and `Foldable` instance for a type can be recovered from the `Traversable` one, just similar to the fact that `Functor` and `Applicative` instance can be recovered from `Monad`.

### Traversable Laws

<table>
<thead>
  <th>Law</th>
  <th>traverse</th>
  <th>sequenceA</th>
</thead>
<tbody>
  <tr>
    <td><b>Naturality</b></td>
    <td><code>t . traverse f = traverse (t . f)</code></td>
    <td><code>t . sequenceA = sequenceA . fmap t</code></td>
  </tr>
  <tr>
    <td><b>Identity</b></td>
    <td><code>traverse Identity = Identity</code></td>
    <td><code>sequenceA . fmap Identity = Identity</code></td>
  </tr>
  <tr>
    <td><b>Composition</b></td>
    <td><code>traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f</code></td>
    <td><code>sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA</code></td>
  </tr>
</tbody>
</table>

## Other Learning Resources 

- [An understandable explanation](https://stackoverflow.com/a/7461112/6347365) on `traverse`

## Recorded Errors & Misconceptions During Doing Exercises

- In chapter exercises:
  - The exercise of writing `Traversable List` instance really makes sense out of Traversable!
  - Still not fully understand how `Traversable Constant` works.
  - Attempted:
  ```Haskell
  instance Traversable Optional where
  [..]
  traverse f (Yep a) = Yep $ f a
  --Expected type: f (Optional b)
  --Actual type: Optional (f b)
  ```
