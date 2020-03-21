data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show)

-- common use of pattern matching
isPartyday :: Weekday -> Bool
isPartyday Fri = True
isPartyday Sun = True
isPartyday _   = False