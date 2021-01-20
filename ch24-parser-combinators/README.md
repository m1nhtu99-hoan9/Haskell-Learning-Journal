# Chapter 24: Parser Combinators

## Reading Notes

### Revision

- Lambda calculus: 
  - "Free variable": variable in body expression of a lambda term that is not named in the head
  - "Combinator": lambda term with no free variables

## Related Learning Resources

- [StackOverFlow: Community's explanation for `some` and `many`](https://stackoverflow.com/q/18108608/6347365)

## Recorded Errors & Misconceptions During Doing Exercises

- My first attempt at `parserDecimalOrFraction`:
  
  ```haskell
  parserDecimalOrFraction :: Parser DecimalOrFraction
  parserDecimalOrFraction = (Left <$> decimal) <|> (Right <$> pureParserFraction)

  ```