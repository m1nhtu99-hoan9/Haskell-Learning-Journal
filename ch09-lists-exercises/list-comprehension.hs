mySqrs :: Int -> [Int]
mySqrs = (map (^ 2) . enumFromTo 1)

myCubes :: Int -> [Int]
myCubes = (map (^ 3) . enumFromTo 1)

serveSomeTuple :: Int -> [(Int, Int)]
serveSomeTuple n = case n > 0 of
  True  -> [(x,y) | 
            x <- (mySqrs n), 
            y <- (myCubes n), 
            y < 50, 
            x < 50]
  False -> (serveSomeTuple . negate) n  

-- @region list comprehension on String as [Char]
acronym :: String -> String
acronym xs = [x | x <- xs, x `elem` ['A'..'Z']] 

doesVowelCheck :: String -> [Bool]
doesVowelCheck "" = error "Why empty string?"
doesVowelCheck xs  = map (\x -> x `elem` "ueoai") xs

-- @endregion 

-- re-implementation of Prelude.length
reLength :: [a] -> Int
reLength []     = 0                   -- base case
reLength (a:xs) = 1 + (reLength xs)   -- recursively calculate length