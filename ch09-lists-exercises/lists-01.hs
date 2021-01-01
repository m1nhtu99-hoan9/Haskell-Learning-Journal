-- review: (:) is `cons` operator, which do prepending

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing 
safeTail (_:xs) = Just xs 