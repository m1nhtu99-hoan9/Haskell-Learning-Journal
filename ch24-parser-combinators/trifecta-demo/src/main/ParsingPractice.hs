module ParsingPractice (
    stop
  , testParse
  , testParse123
  , testEOF
  , parseStringUsingChars
  ) where

import Text.Parser.Char ( CharParsing )
import Text.Parser.Combinators ( eof, choice )
import Text.Trifecta ( 
    Parser
  , unexpected
  , parseString
  , char
  , string 
  )

stop :: Parser a
stop = unexpected "stop"

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "hi"
                                --  ^ monoidal parsing allows more possibilities of polymorphism

{- EXERCISES: PARSING PRACTICE -}

testEOF :: String -> IO ()
testEOF xs = print $ parseString (char 'h' >> char 'i' >> eof) mempty xs

testParse123 :: String -> IO ()
testParse123 xs = print $ parseString p mempty xs
  where 
    -- p = string "123" <|> string "12" <|> string "1"
    p = choice [string "123", string "12", string "1"]
      
parseStringUsingChars :: String -- ^parser literal 
                      -> String -- ^string to be parse
                      -> IO ()
parseStringUsingChars cs xs = print $ parseString (mkPstring cs) mempty xs
  where 
    mkPstring :: (Monad f , CharParsing f) => [Char] -> f [Char]
    mkPstring = mapM char
             -- traverse char :: (Traversable t, Text.Parser.Char.CharParsing f) =>
             --                   t Char -> f (t Char)
             -- mapM char :: (Monad m, Traversable t, CharParsing m) => t Char -> m (t Char)