{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParsingConfigFile (
	parseINI
  , parseSection
  , parseHeader
  , parseAssignment
  , skipComments
  , sampleStr
  ) where

import Control.Applicative ( (*>), (<*), (<|>), some )
import Data.ByteString ( ByteString )
import Data.Char ( isAlpha )
import Data.Map ( Map )
import qualified Data.Map as MapMdl
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text.IO as TextIO
-- import Test.Hspec

import Text.RawString.QQ
import Text.Parser.Char ( letter, oneOf, noneOf )
import Text.Parser.Combinators ( skipMany )
import Text.Trifecta (
    Parser
  , parseString
  , char 
  , decimal 
  , integer 
  )

data Config =
	Config (Map Header Assignment)
  	deriving (Eq, Show)

data Section = 
    Section Header Assignment
    deriving (Eq, Show)

newtype Header = 
    Header String
    deriving (Eq, Show, Ord) 

type Key = String
type Value = String
type Assignment = Map Key Value

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n ")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

rollup :: Section -> Map Header Assignment -> Map Header Assignment
rollup (Section h a) mHa = MapMdl.insert h a mHa

parseINI :: Parser Config
parseINI = do
	scs <- some parseSection
	let mapScs = foldr rollup MapMdl.empty scs
	return (Config mapScs)

parseSection :: Parser Section
parseSection = do
	skipWhitespace
	skipComments
	h <- parseHeader
	skipEOL
	ls <- some parseAssignment
	return (Section h (MapMdl.fromList ls))

parseHeader :: Parser Header
parseHeader = parserSquareBracketPair (Header <$> some letter)
  where
    parserSquareBracketPair :: Parser a -> Parser a
    parserSquareBracketPair p = 
        char '[' *> p <* char ']'
      -- these operators specify that result of the 2 parsing operations
      -- will be discarded, only the parser for the text in between is returned

parseAssignment :: Parser (Key, Value)
parseAssignment = do
    k <- some letter
    char '=' -- perform monadic action and discard the result
    skipEOL  -- perform monadic action and discard the result
    v <- some (noneOf "\n")
    skipEOL
    return (k, v)

skipComments :: Parser ()
skipComments = 
    skipMany (do
        _ <- char ';' <|> char '#'
        skipMany . noneOf $ "\n"
        skipEOL         
      )

sampleINIStr :: ByteString
sampleHeaderStr :: ByteString
sampleAssignmentStatementStr :: ByteString
sampleCommentStr :: ByteString
sampleCommentTrickyStr :: ByteString
sampleStr :: ByteString
sampleINIStr = 
  [r|;comment
  [section]
  host=wikipedia.org
  alias=claw|]
sampleHeaderStr = "[blah]"
sampleAssignmentStatementStr = "blahLevel=1"
sampleCommentStr = 
    "; this comment is not intended\
    \ for this parser to read"
sampleCommentTrickyStr = "; try\n; to read\n    \n; me please"
sampleStr =    
		sampleINIStr <> sampleHeaderStr <> sampleAssignmentStatementStr 
	<>  sampleCommentTrickyStr 
	<>  [r|; ignore me
[states]
Chris=Texas
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]


