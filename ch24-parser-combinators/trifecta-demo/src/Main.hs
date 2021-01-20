{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Ratio ( (%) ) 
import Data.Traversable ( traverse )

import AlternativeParsing ( 
    partialParserFraction
  , parserDecimalOrFraction 
  , pureParserFraction
  )

import Text.Parser.Char ( CharParsing )
import Text.Parser.Combinators ( eof, choice )
import Text.Trifecta ( 
    Parser
  , unexpected
  , parseString
  , char
  , string 
  )

main :: IO ()
main = do
  testParse (char 'h' >> char 'i' >> stop)
  putStrLn mempty
  testParse (char 'h' >> char 'i') 
  putStrLn "✎✎✎✎✎✎✎✎✎✎ EXERCIES: Parsing Practices ✎✎✎✎✎✎✎✎✎✎"
  putStrLn "1. With `eof`: "
  testEOF "hi"
  putStrLn mempty
  testEOF "hi!"
  putStrLn mempty
  putStrLn "2. Parse \"1\", \"12\", \"123\""
  testParse123 "12"
  putStrLn mempty
  putStrLn "✎✎✎✎✎✎✎✎✎✎ Parse Fractions ✎✎✎✎✎✎✎✎✎✎"
  let parseFraction = parseString partialParserFraction mempty
  let parseFraction' = parseString pureParserFraction mempty
  print $ parseFraction' "69/0"
  print $ parseFraction "1/2"
  print $ parseFraction "2/1"
  print $ parseFraction "100"
  -- print $ parseFraction "69/0"
  putStrLn "✎✎✎✎✎✎✎✎✎✎ EXERCIES: Try Try ✎✎✎✎✎✎✎✎✎✎"
  let parseDecOrFrac = parseString parserDecimalOrFraction mempty
  print $ parseDecOrFrac "45/23"

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

