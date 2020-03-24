module Experiments_on_Scan where 
  
  -- @region: Fibonacci sequence
  -- the list itself has to be recursive
  fibs = scanl (+) 1 (1 : fibs) -- non-strictness & lazy evaluation 
  
  fibsN :: Int -> Integer
  fibsN = (!!) fibs

  example1 = takeWhile (< 100) fibs 
  {--
  `example1` expected to be evaluated to `[1,1,2,3,5,8,13,21,34,55,89]` 
  --}