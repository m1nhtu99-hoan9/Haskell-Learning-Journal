module CaseExpressionExercises where
  
  tellSign :: Int -> String
  tellSign 
    = \x -> case (compare x 0) of 
      LT -> "minus"
      GT -> "plus"
      EQ -> "-_-"