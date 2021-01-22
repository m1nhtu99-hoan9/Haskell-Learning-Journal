module ChapterExercises.ParsePhoneNum (
    PhoneNum
  , parsePhone
  ) where

import Control.Applicative
import Data.Traversable ( sequence )

import Text.Trifecta ( Parser )
import Text.Parser.Combinators ( 
    many
  , optional 
  )
import Text.Parser.Char ( 
    digit
  , char
  , string 
  )

type AreaCode = Int
type Exchange = Int
type LineNum = Int

data PhoneNum = 
    PhoneNum AreaCode Exchange LineNum
    deriving (Eq, Show)

parsePhone :: Parser PhoneNum
parsePhone = do
    ac <- parseAreaCode
    ex <- parseExchange
    ln <- parseLineNum
    return (PhoneNum ac ex ln)

parseAreaCode :: Parser AreaCode
parseAreaCode = do
    -- consume "1-" if it's prefixed
    optional (string "1-")
    -- "***-" or "(***) "
    ac <-     intNumDigitsOf 3 <* optional (char '-')
          <|> char '(' *> (intNumDigitsOf 3) <* char ')' <* optional (char ' ')
    return . read $ ac

parseExchange :: Parser Exchange
parseExchange = do
    ex <- intNumDigitsOf 3
    optional (char '-')
    return . read $ ex

parseLineNum :: Parser LineNum 
parseLineNum = do 
    ln <- intNumDigitsOf 4
    return . read $ ln

-- | Parse exactly a number of digits consecutively
intNumDigitsOf :: Int -> Parser String
intNumDigitsOf n = sequence (replicate n digit)
-- intNumDigitsOf = flip Text.Parser.Combinators.count digit
