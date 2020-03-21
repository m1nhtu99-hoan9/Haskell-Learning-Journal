module Employee where 

  data Employee = Coder
                | Manager
                | TeamLeader
                | CEO
                deriving (Eq, Ord, Show)

  makeAClaimOfSuperiority :: Employee -> Employee -> String
  makeAClaimOfSuperiority e e' = unwords [show e, "is the superior of" , show e']

  announce :: Employee -> Employee -> IO ()
  announce e e' = case (compare e e') of 
    GT -> putStrLn $ makeAClaimOfSuperiority e e'
    EQ -> putStrLn "They are counterparts"
    LT -> putStrLn $ (flip makeAClaimOfSuperiority) e e'

