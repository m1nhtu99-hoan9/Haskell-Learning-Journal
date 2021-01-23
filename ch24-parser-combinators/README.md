# Chapter 24: Parser Combinators

## Reading Notes

`[Work in Progress]`

### Revision

- Lambda calculus: 
  - "Free variable": variable in body expression of a lambda term that is not named in the head
  - "Combinator": lambda term with no free variables

### Tokeniser vs Lexer

- Lexers and tokenisers are similar, separating a stream of text into tokens based on indicators such as whitespace or newlines; lexers often attach some context to the tokens,
where tokenizers typically do not.
- Production-grade parsers in Haskell won't use `[]` for performance reasons. Instead, they use `Stream`.

```haskell
lexer :: Stream Char -> Stream Token
parser :: Stream Token -> AST
```
- A mild warning on writing tokeniser: Try to
make it coarse-grained and selective. Overuse of tokenizing
parsers or mixture with character parsers can make your parser
slow or hard to understand.

### Polymorphic Parsers

- Parsers are asserted with polymorphic types (defined by `parsers` library) can work with a plethora of parser libraries.

### Marshalling

- **Marshalling** is the act of preparing data for serialisation, whether via memory (FFI boundary) or over network interface.

```haskell
Text -> Structure -> Meaning -- parse -> unmarshalling
Meaning -> Structure -> Text -- marshall -> serialise
```

- As I comes from JS background, there's no unmarshal/marshall steps because the raw JSON AST will be presented directly as an untyped blob of data.

## Errata

- On eBook page 1531 (book page 1483):

```haskell
data Color = 
    Red String
  | Blue String
  | Yellow String
  deriving (Eq, Show)
```

## Related Learning Resources

- [StackOverFlow: Community's explanation for `some` and `many`](https://stackoverflow.com/q/18108608/6347365)
- [Article "`try a <|> b` considered harmful"](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/)

## Recorded Errors & Misconceptions During Doing Exercises

- My first attempt at `parserDecimalOrFraction`:
  
  ```haskell
  parserDecimalOrFraction :: Parser DecimalOrFraction
  parserDecimalOrFraction = (Left <$> decimal) <|> (Right <$> pureParserFraction)

  ```