newNum :: Int 
newNum = 1

constantOne f = newNum 

addOneIfOdd :: Int -> Int 
addOneIfOdd = \x -> case odd x of 
  True -> (\x -> x + 1) x
  False -> x 