module ProblemEulerOne where
-- Multiples Of 3 and 5
-- source: https://projecteuler.net/problem=1

isDividedBy :: Int -> Int -> Bool
isDividedBy x y = (== 0) (flip mod x y)

multiplesUpto :: Int -> [Int]
multiplesUpto x = filter check [1..x]
  where check = \y -> or [isDividedBy 3 y, isDividedBy 5 y]

sumOfMultiples :: Int -> Int
sumOfMultiples = sum . multiplesUpto 