# hello-project

Example project for chapter 13

## Reading Notes

- `import qualified {...}` so that everything in that module has to be accessed with full namespace
  - `import qualified {...} as {...}` to provide them with alternate name
- `do` is syntactic sugar, is used inside function that return IO in sequence of side effects.
- GHCi tip: `:set prompt "{...}> "`
