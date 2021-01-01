-- @region: factorial implementation 

-- version with guard block
brokenAsF :: Integer -> Integer
brokenAsF n 
  | n == 0    = 1
  | otherwise = n * brokenAsF (n - 1)

-- version with pattern matching
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- @endregion

-- @region: other miscellaneous experiments

  -- pattern matching makes recursion expression more succint
incTimes :: (Num t, Eq t) => t -> t -> t
incTimes n 0       = n
incTimes n anyNum  = n + (incTimes n (anyNum-1))

type Result = Integer
data DividedByZero = Exception
data DividedResult = Result Integer | DividedByZero

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy numer denumer = exec numer denumer 0
  where exec nMatch dMatch cMatch 
    | dMatch == 0     = (error $ "Divided by 0 !!!")
	  | nMatch < dMatch = (cMatch, nMatch) -- tuple of quotient & remainer
	  | otherwise 	    = (exec (nMatch-dMatch) dMatch (cMatch+1))  
-- @endregion


