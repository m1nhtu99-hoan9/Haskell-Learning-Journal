module List_Transformation where
  import Data.Bool

  -- reimplement `map` for research purpose
  myMap :: (a -> b) -> [a] -> [b]
  myMap _ []      = []
  myMap fn (x:xs) = fn x : (myMap fn xs)

  a = myMap 
    (\x -> case odd x of 
      True -> x + 1 
      False -> x * 2) 
    [1..784]

  -- experiment with Data.Bool.bool
  -- bool f t p == if p then t else f
  numify :: [Int] -> [Int]
  numify = map (\x -> bool (x*2) (x+1) (even x))
