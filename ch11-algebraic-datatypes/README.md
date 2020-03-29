# Reading Notes on Chapter 11

```
type constructors -- compile time
----------------- -- phase separation
data constructors -- runtime
```

## Review from Chapter 4 to 6

- Type aliases create type constructors, not data constructors. For instance: `type TwoBools = (Bool, Bool)`

## Distinguish between Data and Type

- When data constructor takes arguments, these arguments refer to other types

## Data Constructor Arities

- _"-ary"_ suffix means "of or pertaining to"
- Tuples are _anonymous products_ (because they don't have name)

## How datatypes are algebraic?

- **cardinality** of a datatype is the number of possible
  values it defines

### Function type is exponential

- function type `foo :: a -> b` have number of inhabitants of (cardinality of `b`) `^` (cardinality of `a`)

## `newtype`

- `newtype` vs. type synonym/alias (such as `String` is type alias of `[Char]`):
  - similar: representation of the named type and the type it contains are identical; any distinction will be gone at compile time
  - difference: type synonyms can't have difference typeclass instance definition, but type constructed by `newtype` and its contained type can

## Sum Types & Product Types

- Cardinality of _sum types_ are calculated as sum of cardinalities of their type constructors
- _Product type_ gives a way to encapsulate 2/more pieces of data in a single value

### Normal form

- Distributive property: _Product_ distributes over sums: `a*(b+c) = a*b + a*c`
- **Normal form** in data declaration need to demonstrate type as sum of products in term of types

## Trivias

- As-pattern
