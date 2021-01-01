module List_Filtering where

  -- reimplement `filter` for research purpose
  myFilter :: (a -> Bool) -> [a] -> [a]
  myFilter _ [] = []
  myFilter predicate (x:xs)
    | predicate x = x : (filter predicate xs)
    | otherwise   = filter predicate xs

  noArticles :: String -> String
  noArticles "" = error "We don\'t do empty string here!!!"
  noArticles x  = (unwords . ((filter (\x -> not (elem x ["the", "a", "an"]))) . words)) x 