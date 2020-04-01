module Hello 
  ( yell )
  where 

  yell :: String -> IO()
  yell name = do 
    putStrLn ""
    putStrLn "HELLO THE MOTHERFUCKERS"
    putStrLn (concat ["Are you a motherfucker too, ", name, "?"])