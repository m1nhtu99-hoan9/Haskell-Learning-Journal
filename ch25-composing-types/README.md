# Chapter 24: Composing Types

## Reading Notes

### Revisions

- `newtype` has underlying representation identical to the type it represents
- `fmap . fmap` technique to apply a unary function `f` to multiple functorial structure.
- *Functors* and *Applicatives* are both closed under composition, which means that 2 functors (or 2 applicatives) can be composed to produce another functor (or applicative).

### `Identity` and `Compose`

- A *monad transformer* is a variant of an ordinary type that takes an additional type argument which is assumed to have a `Monad` instance.

- The kind signature of `Identity` type constructor is similar to type signature of `id` function (although the fidelity of the comparison isn’t perfect given the limitations of type-level computation in Haskell):
  
```haskell
Prelude> :t id
id :: a -> a
Prelude> :k Identity
Identity :: * -> *
```

- `Compose` type constructor

```haskell
Prelude Data.Functor.Compose> :i Compose
type role Compose representational nominal nominal
newtype Compose (f :: k -> *) (g :: k1 -> k) (a :: k1)
  = Compose {getCompose :: f (g a)}
  	-- Defined in ‘Data.Functor.Compose’
infixr 9 `Compose`
{- ... more ... -}
Prelude Data.Functor.Compose> :t Compose [Just "hello", Nothing, Just "types"]
Compose [Just "hello", Nothing, Just "types"]
  :: Compose [] Maybe [Char]

```

- Composing 2 monads doesn't give another monad
- `Identity` and `Compose` types
- Mannipulate types until we can make monads compose
- Meet some common monad transformers
- `Identity` crisis

## Recorded Errors & Misconceptions During Doing Exercises

- In intermission exercise of writing `Applicative` instance for `Compose f g`:

```haskell
instance (Applicative f, Applicative g) => Applicative (Compose f g) where 
    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    Compose fgh <*> Compose fga = Compose $ ((<*>) . (<*>)) fgh fga
```


