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
import ParsingPractice (
    stop
  , testParse
  , testParse123
  , testEOF
  , parseStringUsingChars
  ) 
import ParsingConfigFile (
    parseINI
  , parseSection 
  , parseHeader
  , parseAssignment
  , skipComments
  , sampleStr
  ) 
import ChapterExercises.SemVer (
    SemVer
  )

import Text.Trifecta (
    parseString
  , parseByteString
  , char 
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
  print $ parseDecOrFrac "23.4"
  putStrLn "✎✎✎✎✎✎✎✎✎✎ Parse INI Config File ✎✎✎✎✎✎✎✎✎✎"
  print $ parseByteString parseSection mempty sampleStr
  print $ parseByteString parseINI mempty sampleStr
