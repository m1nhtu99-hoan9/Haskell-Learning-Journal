# Chapter 23: State

## Reading Notes

### Review 

- Function accessor contained in the `newtype` must be isomorphic to the type it wraps

  ```haskell
  -- example of isomorphism
  newtype Sum a = Sum { getSum :: a }
  
  -- counter-example of isomorphism
  type Iso a b = (a -> b, b -> a)
  ```

### State 

```haskell
newtype State s a = State { runState :: s -> (a, s) }

-- therefore
State :: (s -> (a, s)) -> State s a
runState :: State s a -> s -> (a, s)
```

- `State` takes input state and returns an output value `a`, tupled with the new state value

## Related Learning Resources

- [Short online free course](https://academy.mondaymorninghaskell.com/p/your-first-haskell-project) teaches how to create, manage & build Haskell projects using `Cabal`, `Stack`, `hpack`.

## Recorded Errors & Misconceptions During Doing Exercises
