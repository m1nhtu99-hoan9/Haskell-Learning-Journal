--MONAD LIST
{-
  (>>=) :: [a] -> (a -> [b]) -> [b]
-} 

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do 
  x <- xs -- xs@([x]) >>= \x -> even x ...
  -- the binding performed there is somewhat like a list comprehension
  if even x 
    then [x*x, x*x]
    else [] -- odd values evaporated into the void {just kidding @@}

--MONAD MAYBE 

--MONAD EITHER