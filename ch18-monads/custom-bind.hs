import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . (fmap f)

-- `join` helps to flatten the layers of structure

{-
*Main> oneMore = \x -> [x, 1]
*Main> bind oneMore [1,5,9]
[1,1,5,1,9,1]
-}

binding :: IO ()
binding = do 
  name <- getLine
  putStrLn (mconcat [name, " is awesome"]) 

binding' :: IO ()
binding' = 
  getLine >>= 
    \x -> putStrLn $ mconcat [x, " is awesome"]