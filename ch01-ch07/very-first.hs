greet :: String -> IO ()
greet input = putStrLn ("Hello " ++ input ++ " the Motherfucker!")
-- first shit
someOps :: IO ()
someOps = do 
  putStrLn x
  putStr y
  where 
    x = "This shit\'s"
    y = concat [x," so real!"]

-- experiment with string
-- actually String is semantic sugar for [Char]
charAtIndex :: Int -> String -> Char 
charAtIndex x y = y !! x

-- experiment: show length plus one
calcLength :: String -> Int
add = (+)
calcLength xs = w `add` 1 
  where w = length xs

-- experiment: using where to make type definition
-- not recommended 
triple x = tripleInside x 
  where tripleInside :: Int -> Int 
        tripleInside = \y -> y*3

-- `data` keyword and record syntax 
-- https://mmhaskell.com/blog/2017/12/24/haskell-data-types-in-5-steps

-- recursion demo

recurseHello :: Int -> String
recurseHello n 
  | n == 0    = "Hello Bullshit"
  | otherwise = recurseHello (n - 1) ++ "\nHello Bullshit"

  
