# Chapter 22: Reader

[Imma halt my Haskell studying at this point. This point onward covers a lot of advanced topics that I don't feel ready yet.]

## Reading Notes

### Re-thinking 

- Functions `* -> *` have `Functor`, `Applicative` and `Monad` instances.
  - `Functor` of functions is function composition: 
    - `f . g` is identical to `fmap f g` where
    
    ```haskell
    -- For example:
    f :: a -> b
    g :: b -> c
    ``` 
  - `Applicative` & `Monad` instance for function type give a way to map function that is awaiting an `a` over another function that is also awaiting an `a`.
    - An `ap` technique to exploit when 2 unary functions are recognised to share the same input, and then their output need to applied to a binary function.
  
    ```haskell
    Prelude> ((+) <$> (*2) <*> (/2)) 30
    75.0
    Prelude Control.Applicative> (liftA2 (+) (*2) (/2)) 30
    75.0
    ```
     
- `Reader` often refers to the `Monad` instance.
  
  ```haskell
  (>>=) :: Monad m => 
     m    a  -> (a -> (m    b)) ->  m    b
    (r -> a) -> (a -> (r -> b)) -> (r -> b)
  -- explicitly: (->) r a -> (a -> (->) r b) -> (->) r b 
  
  return :: Monad m => a -> m    a
  return ::            a -> r -> a
  -- explicitlyL:      a -> (->) r a                
  ```

  - An example of combine `Traversable`'s `sequenceA` and `Reader` in surprise way

    ```haskell
    Prelude Data.Traversable> sequenceA [(> 3), (< 8), even] 7
    [True,True,False]
    ```

- `Reader` is an answer to **Dependency Injection** problem.


## Related Learning Resources
- [A tutorial](https://gist.github.com/twopoint718/c02164137c6fca9e0c4c) attempts to explain `Reader`. The author also introduced solutions to some of this chapter's exercises
- [This *Tsoding*'s stream record](https://www.youtube.com/watch?v=N9RUqGYuGfw) also accompany this chapter's content. The parser _Tsoding_ coded is a Reader. 
## Recorded Errors & Misconceptions During Doing Exercises

- My first attempt at writing `bind` implementation for `Monad (Reader r)`:
  ```haskell
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader f) >>= fRg = Reader (\r -> fRg (f r))
  ```