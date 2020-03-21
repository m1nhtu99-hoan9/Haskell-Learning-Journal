module Reimplemtations_Standard_Functions where
  
  myFoldr :: (a -> b -> b) -> b -> [a] -> b
  myFoldr f z xs = 
    case xs of 
      []        -> z
      (y : ys)  -> f y (myFoldr f z ys)