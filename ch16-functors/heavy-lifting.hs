a = fmap (+1) $ read "[1]" :: [Int]
--  a == [2]

b = (fmap . fmap) (++ "lol") (Just ["Hi, ", "Hello"])
-- b == Just ["Hi,lol","Hellolol"]

c = (*2) . (\x -> x - 2)
-- c 1 = -2

d = ((return '1' ++) . show) . (\x -> [x, 1..3])
-- d 0 = "1[0,1,2,3]"
--  `return` has type signature of `Monad m => a -> m a`
--   btw, wtf is Monad? 

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer 
        changed = read <$> (fmap ("123" ++) (fmap show ioi))
    in fmap (*3) changed

-- first step: change `show ioi` to `fmap show ioi`
--      ; after this step: IO Integer -> IO String
-- second step: change `("123" ++) (fmap show ioi)` to 
--                     `fmap ("123" ++) (fmap show ioi)`
--      ; after this step: IO String -> IO String
-- third step: change `read $ {...}` to `read <$> {...}`
--      ; after this step: IO String -> IO Integer
-- last step: change `in (*3) changed` to `in fmap (*3) changed`
--      so that (Integer -> Integer) -> IO Integer -> IO Integer