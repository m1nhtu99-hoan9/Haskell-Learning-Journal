# Chapter 27: Nonstrictness

## Reading Notes

- live the Thunk Life
- consider runtime behaviour of non-strict code in termns of sharing
- develop methods for observing sharing behaviours and measuring program efficiency
- bottom out the bottoms

### How Nonstrict is Haskell

- Strict languages evaluate *inside out*. Nonstrict languages evaluate *outside in*, which means evaluation proceeds from the outermost parts of the expressions and works inward based on what values are forced.
- Expressiveness also means that we can refer to values before we've done the work to create them.
- Case matching in pattern matching also does force and chain evaluation.
- **Tips & Tricks**: 
  - Injecting bottom everywhere allows seeing clearly what's being evaluated strictly and what's not.
  - `-ddump-simpl` to inspect [`Core` dump](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type)

### Evaluation Strategies: Call-by-Name vs Call-by-Need

- *Call-by-value*: inside-out evaluation, expresions are evaluated before creating the binding.
- *Call-by-name*: outside-in evaluation; expressions can be arguments to other functions without having been evaluated. 
- *Call-by-need*: the same as *call-by-name*, plus that expressions are only evaluated once. GHC takes this approach.
  
### Thunk 

- A *Thunk* is used to reference suspended computation that might be performed or computed at a later point in your program. In other words, thunks are computations not yet evaluated to *WHNF* (weak head normal form).
- *Data constructors* are different to *functions*: 
  - Data constructors are like functions when they're unapplied, and constants once they're fully applied.
  - *GHC* will not thunk data constructors.
- **Tips & Tricks**: To see if a thing is thunked or not in `GHCi`, use `:sprint {thing}` 

### Sharing

- `trace` function in `base` module `Debug.Trace` is a rudimental method for observing sharing 
- To force sharing: 
  - Give to-be-shared expression a name
  
  ```hs
  forever :: (Monad m) => m a -> m b
  forever a = let a' = a >> a' in a' 
  -- the sharing here cause GHC to overwrite the thunk as it runs each 
  -- monadic action in the evaluation, thus doesn't leak memory
  ```

- To subvert or prevent sharing (when we, purposefully, don't want a large datum hanging out in in memory):
  - *Inlining* expressions (similar to "inline" concept in C). *Caveat*: The expression to be inlined need to atomic.
  - *Typeclass constraints* (because they are functions in `Core`)

  ```hs
  -- example
  Prelude> let blah = Just 1
  Prelude> :t blah
  blah :: Num a -> Maybe a
  Prelude> fmap ((+1) :: Int -> Int) blah
  Just 2
  Prelude> :sprint blah
  blah = _
  ```

    - *Implicit parameters* (through compiler extension `-XImplicitParams`) are implemented similarly to typeclass constraints (thus, having the same effects on sharing)
  - Functions aren't shared when they are named argument; but are shared when the arguments are elided (as in pointfree)

- Under the hood, after type class constraints get simplified to the underlying `GHC Core` language, they're really function arguments. Moral of the story:
> Polymorphic expressions can't be shared. Polymorphic values maybe evaluated once but still not shared because, underneath, they continue to be functions awaiting application.

### Irrefutable & Refutalble Patterns 

- `~` (tilde) notation
  - *Why?* Forcing before evaluating function body promotes predictability of memory usage and performance 
- Lazy patterns are also irrefutable

### Bang Patterns

```hs
{-# LANGUAGE BangPatterns #-}
```

- `!` (exclamation mark)
  - *Why?* Sometimes it's cheaper to just compute something than to construct a thunk and then evaluate it later (for example, in numerics code where a lot of `Int` or `Double` runs a round which are individually cheap to conjure).
- A rule to follow: **Lazy in the spine, strict in the leaves!**

### Strict and StrictData

- GHC >= 8.0: `Strict` and `StrictData` extensions
- Ways to avoid putting in pervasive uses of `seq` and `bang`. In other words, use `Strict` and `StrictData` extensions in modules where everything or almost everything supposed to be strict.

## Related Learning Materials

- ["Lazy Evaluation Illustrated for Haskell Divers"](https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf), a visual learning resource by *Takenobu Tani*
- [Youtube video](https://www.youtube.com/watch?v=QBQ9_9R7o8I&t) exlains Weak Head Normal Form
- [Heap Objects in *GHC*](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects)
- [Purescript GitHub issue discussing how to implment inlining](https://github.com/purescript/purescript/issues/2345#issuecomment-279171865)
  
## Recorded Errors & Misconceptions During Doing Exercises