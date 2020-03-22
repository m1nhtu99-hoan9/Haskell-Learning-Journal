module How_to_Write_Folds where
  
  curlyWrap :: (String, String) -> String
  curlyWrap (x, y) = concat ["(", x, y, ")"]

  acro3chars :: Int -> [String] -> IO()
  acro3chars n = case n of 
    0 -> putStrLn . foldr (\a b -> curlyWrap ((curlyWrap ("take 3 ", a)), b)) ""  --visual demo
    1 -> putStrLn . foldl (\a b -> curlyWrap ((curlyWrap ("take 3 ", b)), a)) ""  --visual demo
    2 -> putStrLn . foldr (\a b -> take 3 a ++ b) ""  --fold rightward, value accumulated on `b`
    3 -> putStrLn . foldl (\a b -> take 3 b ++ a) ""  --fold leftward, value accumulated on `a`
    _ -> error $ "What cha tryna do?"