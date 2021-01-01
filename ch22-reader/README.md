# Chapter 22: Reader

[Imma halt my Haskell studying at this point. This point onward covers a lot of advanced topics that I don't feel ready yet.]

## Reading Notes

- Functions `* -> *` have `Functor`, `Applicative` and `Monad` instances.

  - `Functor` of functions is function composition.
  - `Applicative` & `Monad` instance for function type give a way to map function that is awaiting an `a` over another function that is also awaiting an `a`.

- `Reader` often refers to the `Monad` instance.
- `Reader` is an answer to **Dependency Injection** problem.

### Breaking Down the `Functor` of Functions

```Haskell
instance Functor ((->) r) where
  fmap = (.)
```

### `Reader`

- `Reader` is newtype wrapper for the function type

## Recorded Errors & Misconceptions During Doing Exercises
