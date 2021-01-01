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
    
    -- `Maybe` catamorphism
  mayybee :: b -> (a -> b) -> Maybe a -> b
  mayybee i f x = case isNothing x of
    True  -> i
    False -> let Just _x = x in f _x

  justifise :: [Maybe a] -> [Maybe a]
  justifise = filter (not . isNothing)

  catMaybes :: [Maybe a] -> [a]
  catMaybes = map (\j -> let Just j' = j in j') . justifise 

  hasNothings :: [Maybe a] -> Bool
  hasNothings = not . null . snd . break isNothing

  flipMaybe :: [Maybe a] -> Maybe [a]
  flipMaybe xs = case hasNothings xs of 
    True  -> Nothing 
    False -> Just (catMaybes xs) 

  -- @endregion

  -- @region: Small library for `Either`
  isEitherLeft :: Either a b -> Bool
  isEitherLeft (Left _) = True
  isEitherLeft _        = False

  leftVal :: Either a b -> a
  leftVal x = x' 
    where Left x' = x

  rightVal :: Either a b -> b
  rightVal x = x'
    where Right x' = x  
  
  lefts :: [Either a b] -> [a]
  lefts = map leftVal . filter isEitherLeft

  rights :: [Either a b] -> [b]
  rights = map rightVal . filter (not . isEitherLeft)

  halves :: [Either a b] -> ([a], [b])
  halves es = (lefts es, rights es)
  
  -- @endregion 

  -- @region: testing setup

  type Number = Either Int String 
  
  listOfNums :: [Number]
  listOfNums =  [Left 15, Right "eight", Left (negate 69), Right "one"]

  -- @region