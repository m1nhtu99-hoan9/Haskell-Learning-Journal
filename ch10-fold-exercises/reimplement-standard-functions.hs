module Reimplemtations_Standard_Functions where
  
  myFoldr :: (a -> b -> b) -> b -> [a] -> b
  myFoldr f i xs = 
    case xs of 
      []        -> i
      (y : ys)  -> f y (myFoldr f i ys)

  myFoldl :: (a -> b -> b) -> b -> [a] -> b
  myFoldl f acc xs = 
    case xs of 
      []        -> acc
      (y : ys)  -> myFoldl ((f acc y) `f` ys)
