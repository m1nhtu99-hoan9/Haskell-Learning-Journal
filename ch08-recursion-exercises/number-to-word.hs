module NumbersToWords where
  import Data.List (intersperse)

  digitToWord :: Integer -> String
  digitToWord x = case x of 
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    -- no need to match `else` case, as GHC will handle it

  digits :: Integer -> [Integer]
  digits n 
    | n < 10    = [n]
    | otherwise = digits (quot n 10) ++ [rem n 10]

  numberToWord :: Integer -> String
  numberToWord n 
    | n > 0 = (concat . intersperse "-") (map digitToWord (digits n))
    | n < 0 = "negative-" ++ numberToWord (abs n)  
  
  -- try: `(numberToWord . negate) 48141541474121414124142`
