module ChapterExercises.SemVer (
    SemVer
  , parseNonLeadingZeroInteger 
  , parseSemVer
  ) where

import Control.Applicative
import Data.Char ( ord )
import Text.Trifecta

import Text.Parser.Combinators ( 
    (<?>)
  , notFollowedBy
  , many
  , skipMany 
  , eof
  )
import Text.Parser.Char ( 
    CharParsing
  , satisfy
  , oneOf
  , letter
  , digit
  , char 
  )

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
    SemVer Major Minor Patch Release Metadata 
    deriving (Eq, Show)

instance Ord SemVer where
    compare (SemVer mj1 mn1 pch1 rl1 _) (SemVer mj2 mn2 pch2 rl2 _) = 
        case compare mj1 mj2 of
            EQ -> case compare mn1 mn2 of
                EQ -> case compare pch1 pch2 of 
                    EQ -> compare rl1 rl2
                    b2@(_) -> b2
                b1@(_) -> b1        
            b0@(_) -> b0

parseSemVer :: Parser SemVer
parseSemVer = do
    -- parse major
    mj <- parseNonLeadingZeroInteger
    char '.'
    -- parse minor
    mn <- parseNonLeadingZeroInteger
    char '.'
    -- parse patch
    pch <- parseNonLeadingZeroInteger
    -- parse empty array or list of `NumberOrString`
    rl <- (char '-' *> parseSomeNos) <|> pure []
    -- parse empty array or list of `NumberOrString`
    mt <- (char '+' *> parseSomeNos) <|> pure []
    eof  
    return (SemVer mj mn pch rl mt)

parseSomeNos :: Parser [NumberOrString]
parseSomeNos = some (skipMany (char '.') *> parseNos)

parseNos :: Parser NumberOrString
parseNos = 
      NOSS <$> some letter 
  <|> NOSI <$> parseNonLeadingZeroInteger

-- parseNonLeadingZeroInteger' :: Parser String
-- parseNonLeadingZeroInteger' = do
--     -- only consumes the first character if it's non-zero
--     d1 <- oneOf ['1'..'9']
--     d2 <- many digit <* notFollowedBy letter
--     return $ d1 : d2

parseNonLeadingZeroInteger :: Parser Integer
parseNonLeadingZeroInteger = do
    x <- nonZeroDigit
    -- `notFollowedBy` doesn't consume input, but it ensure that
    -- the parser succeeds only when the string to parse doesn't end with a letter
    xs <- many digit <* notFollowedBy letter
    return . read $ x : xs

-- | Parse a non-zero digit. Returns the parsed character inside a monadic structure.
nonZeroDigit :: CharParsing m => m Char
nonZeroDigit = satisfy isNonZeroDigit <?> "non-zero digit"

-- | Return @True@ for ASCII digits of @\'1\'@..@\'9\'@.
isNonZeroDigit :: Char -> Bool
isNonZeroDigit c = 
  let d = (fromIntegral (ord c - ord '0') :: Word)
  in d > 0 && d <= 9

