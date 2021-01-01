module Main where
  import Hello
  import System.IO

  main :: IO () 
  main = do
    hSetBuffering stdout NoBuffering
    putStr "Give me a name: "
    -- `<-` is called `bind`
    name <- getLine
    yell name 
