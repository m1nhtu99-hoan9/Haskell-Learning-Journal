# Chapter 28: Basic Libraries

- Evaluate time & space usage of a Haskell program
- Guidelines on when WHNF or normal form are appropriate when benchmarking code
- Define *constant applicative forms* & explain argument saturation
- Demonstrate & critically evaluate when to use different data structures in different circumstances
- Sacrifice some jargons for the jargon gods

## Reading Notes

### Revisions

- Lists are as much control structures as data structures. It's very cheap in GHC to construct a list which is thrown away immediately.
  - As discussed below, lists are cheaper in term of memory comsumption than CAFs
- *Boxed* vs *unboxed* types: 
  - Most types in Haskell are *boxed*, which mean the value of the type are represented as a pointer to a heap object
  - *Unboxed* types correspond to "raw machine" types used in `C`

### 1. Benchmarking

- [`criterion`](https://hackage.haskell.org/package/criterion) package
  
- When to compile code for benchmarking
  ```bash
  $ # with stack
  $ stack ghc -- -O2 to-be-compiled.hs
  $ # without stack
  $ ghc -O2 to-be-compiled.hs
  ```
- Tips to get more realistic benchmark result: 
  - Force bottom on `whnf` and `nf`
- Side note: If benchmark results get weird, wiping the build is needed (`stack clean` or `cabal clean`)
  
#### When WHNF and When Normal Form?

- *WHNF*: 
  - In benchmarking with `criterion`, use `whnf` when the first outemost data structure is a meaningful indicator of whether the work you're interested in has been done. 
  - E.g. If the outermost data structure is `Maybe`, it would be enough to know if the value is `Nothing` or a `Just`. For a `Just a`, the cost of evaluating `a` value is not counted.
- Normal form:
  - As constrast to the benchmarking using `whnf`, use `nf` when measurement of the cost of evaluating to normal form is meaningful. 

### 2. Profiling 

- *Benchmarking* is to inspect how fast our programs are. *Profiling* is to inspect why they're slow or fast and where they're spending their time.

- To compile this folder's `large-CAF-demo.hs` for profiling:

  ```bash
    $ # Without `stack`, `GHC` will fail to acknowledge `Prelude`
    $ stack ghc -- -prof -fprof-auto -rtsopts -O2 ./large-CAF-demo.hs 
    $ # Profiler output will be dumped to `large-CAF-demo.prof`
    $ ./large-CAF-demo +RTS -hc -p
    $ # To produce postscript file (can be opened with any PDF reader) demonstrates
    $ # how much memory the program used in its runtime
    $ hp2ps ./large-CAF-demo.hp
  ```

  - `-prof` enables profiling
  - `-fprof-auto` assigns all bindings not marked `inline` a cost center named after the binding
  - `-rtsopts` enables to pass GHC RTS options to the generated binary (to get smaller binary if desired)
  - `-O2` enables highest level of optimisations
  
### 3. Constant Applicative Forms (CAFs)

- *CAF* is also a critical concepts in the discussion of memory usage and sharing in Haskell.
- *CAFs* are expressions that have no free variables and are held in memory to be shared with other expressions in a module (i.e. literal values or partially applied functions that have no named arguments).
- Pros and Cons:
  - [+] CAF can make some programs faster since you don't have to keep re-evaluating shared values
  - [-] If a program uses much more memory than expected, then surely a CAF is lurking somewhere.
- To counter CAF:
  - Profile the program to find the CAF and kill it 
  - Change pointfree top-level declaration (which is the CAF) to pointful ones.
- Possibility of CAFs existing in production code is rare because data is often pulled from somewhere.

### 4. Collection Data Structures

#### Map (from `containers`)

- `Map` represent associations of unique pairigns of keys to values (alternative to association list of type `[(a, b)]`)
- Fast for: looking up by key
- Slow for: key is numeric value 
  - Alternatives: `HashMap`, `IntMap`, `Vector`

#### Set (from `containers`)

- Unique, ordered set of values
- Same pros and cons as `Map`

#### Sequence (from `containers`)

- Built on top of finger trees 
- Fast for: 
  - (most known for) Append to both ends (have cheaper and more efficient access to tail than *list*)
  - (most known for) Concatenation
  - traverse and index (in compare to *list*)
- Disadvantages:
  - Memory density isn't as good as `Vector` (because both `Sequence` and `Map` are persistent data structures)
  - Slow for indexing by `Int` (alternative: `Vector`)

#### Vector (from `vector`)

- Efficient alternative to default `Array` in Haskell
- *Unboxed* version of `Vector` is only limited to types like `Bool`, `Char`, `Double`, `Float`, `Int`, `Word` & tuple of unboxable values.
- `Vector` is desired when: 
  - In need of memory efficiency close to theoretical maximum for the data been worked with
  - Data access is almost exclusively in terms of indexing via an `Int` value
  - Time complexity for operation of accessing each element is `O(1)`
  - In need to construct a collection once and read it many times. For ongoing, efficient updates, consider mutable `Vector`
  - Cheap slicing 

### 5. String types 

#### `String`

- Type alias for `[Char]`
  
#### `Text`

- Extension `OverloaddedStrings`
- Benefits: 
  - Compact representation in memory
  - Efficient indexing into the string
- Encoded as `UTF-16`. However:
  - `UTF-16` is often faster due to cache friendliness (via chunkier and more predictable memory accesses).
  
#### `ByteString`

- Sequences of bytes represented (indirectly) as a vector of `Word8` values.
- Encoding can be `ASCII`, `UTF-8`, `UTF-16`, `UTF-32` (usually `UTF-8` and `UTF-16`)
- Pros: Helps to address more broader problem space than mere text.
- Cons: Bytes of data isn't comprehensible text.

## Related Learning Materials

  
## Recorded Errors & Misconceptions During Doing Exercises