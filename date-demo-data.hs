data DayOfWeek = 
  Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Ord, Show)

data Date = Date DayOfWeek Int

instance Show Date where 
  show (Date (d1) (d2)) = unwords [show d1 ++ ".", "Day", show d2, "of the month."]

askForParty :: DayOfWeek -> String
askForParty x
  | x < Fri     = "Work your ass off!"
  | x > Sat     = "Play harder!"
  | otherwise   = "What to do on a moody day?"
