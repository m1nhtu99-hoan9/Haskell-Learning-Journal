module Chap12.Exercises where
  import Data.List
  type StringWord = String

  -- @region: String Processing
    -- notThe exercises expects all input strings are in uppercase

  isVowel :: Char -> Bool
  isVowel = or . flip map "aeiou" . (==)

  notThe :: StringWord -> Maybe StringWord
  notThe "the" = Nothing
  notThe x     = Just x

  replaceThe :: String -> String
  replaceThe = unwords . (map f) . words
                where f = \w -> case notThe w of
                            Nothing -> "a"
                            Just x -> x

  isInitiatedByVowel :: String -> Bool
  isInitiatedByVowel = isVowel . head

  indicesOfThe :: String -> [Int]
  indicesOfThe = elemIndices "the" . words

  countTheBeforeVowel :: String -> Int
  countTheBeforeVowel xss = length (filter f (indicesOfThe xss))
                          where f = isInitiatedByVowel . ((!!) (words xss)) . (+ 1)
  
  countVowelChars :: StringWord -> Int
  countVowelChars = length . filter isVowel  
  -- @endregion

  -- @region: Small library for `Maybe`

  isNothing :: Maybe t -> Bool
  isNothing Nothing = True
  isNothing _       = False

  mayybee :: b -> (a -> b) -> Maybe a -> b
  mayybee 

  -- @endregion