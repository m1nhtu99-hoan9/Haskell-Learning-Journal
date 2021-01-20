{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AlternativeParsing (
    partialParserFraction
  , pureParserFraction 
  , parserDecimalOrFraction
  ) where

import Control.Applicative

import Data.Ratio ( (%) ) 

import Text.RawString.QQ 
import Text.Parser.Combinators ( skipMany )
import qualified Text.Parser.Combinators as ParserCombinators
import Text.Parser.Char ( letter, oneOf )
import Text.Trifecta (
    Parser
  , parseString
  , char 
  , decimal 
  , integer 
  )

type NumberOrString = Either Integer String
type DecimalOrFraction = Either Integer Rational
data Name = 
    Name String
    deriving (Show, Eq)

main = do 
    let p f i = parseString f mempty i
    print $ p parserNumberOrString strQQ
    print $ p (some parserNumberOrString) strQQ
    print $ p parserName "Thomas"

parserNumberOrString :: Parser NumberOrString
parserNumberOrString = 
    -- discard newline and space character
    skipMany (oneOf "\n ") 
    >> 
        (Left <$> integer) 
    <|> (Right <$> some letter)
        --  ^ some letter :: CharParsing f => f [Char]

-- | to cope with spurious terminal newlines
parserRuthlessNumOrString :: Parser NumberOrString
parserRuthlessNumOrString = do 
    skipMany (oneOf "\n ")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n ")
    return v

parserName :: Parser Name
parserName = Name <$> (some letter)

strQQ :: String
strQQ = [r|
123
abc
456
def|]

{- PARSING FRACTIONS -}

-- | Rudimentary version which can't handle bottom value
partialParserFraction :: Parser Rational 
partialParserFraction = do
  -- parse numerator
  pN <- decimal -- decimal :: Integral a => Parser a -- simplified type signature
  -- parse '/'
  char '/'
  -- parse denominator
  pD <- decimal 
  -- lift the result `Rational` to `Parser Rational`
  return (pN % pD)

-- | Virtuous version
pureParserFraction :: Parser Rational
pureParserFraction = do 
  -- parse numerator
  pN <- decimal 
  -- parse '/'
  char '/'
  -- parse denominator
  pD <- decimal 
  case pD of 
    0 -> fail "Denominator cannot be zero" -- fail :: Monad m => String -> m a
    _ -> return (pN % pD)

{- EXERCISES: TRY TRY -}

parserDecimalOrFraction :: Parser DecimalOrFraction
parserDecimalOrFraction = 
  -- the more specific parser should be matched 
  -- before the parser handling more broad range of patterns
      ParserCombinators.try (Right <$> pureParserFraction)
  <|> ParserCombinators.try (Left  <$> decimal) 






