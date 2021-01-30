# Chapter 29: IO

## Reading Notes

### Revision

- *GHC* is ordinarily free to do a lot of re-ordering of operations, delaying of evaluation, sharing of named values, duplicating code via inlining, and other optimisations.
- *Monad* abstraction enables nested lambdas
- *Referential Transparency*: 
  > A context in a sentence is "referentially transparent" if replacing a term in that context by another term that refers to the same entity doesn't alter the meaning. 
  [(source)](https://stackoverflow.com/a/9859966/6347365)
  
### `IO`

- `IO` is a datatype that disallows sharing in some cases and gives us the way to order operations. 

```hs
newtype IO = 
    IO (State# RealWorld 
        -> (# State# RealWorld, a #))
```

- `RealWorld` uses zero bits of memory. The state tokens underlying the `IO` types are eased during compile time, which add no overhead to the runtime (so it's not the `State` we can meaningfully interact with).

- `IO a` doesn't guarantee that the computation produces return value of type `a`.
- Values not dependent on `IO` for its evaluation can still be shared.

## Related Learning Resources

- "Parallel and Concurrent Programming in Haskell" book by Simon Marlow
