module Chapter10_Exercises where
  import GHC.Char

  stops = "pbtdkg"
  vowels = "aeiou"

  combinations = [ a : b : c : [] | a <- stops, b <- vowels, c <- stops ]

  {--
  ``` "pad" `elem` combinations ``` expects `True`
  ``` "dog" `elem` combinations ``` expects `True`
  ``` "gig" `elem` combinations ``` expects `True`
  --}

  combsStartWithP = filter (eqChar 'p' . head) combinations 