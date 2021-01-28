{-# LANGUAGE Strict #-}

data List a =
      Nil
    | Cons a (List a)
    deriving (Show)

take' :: Int -> List a -> List a
take' n _ 
    | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) =
    (Cons x (take' (n-1) xs)) 

map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (Cons x xs) = 
    (Cons (f x) (map' f xs))

repeat' :: a -> List a
repeat' x = xs where xs = (Cons x xs)
  -- in default non-strict code, using `where` forces GHC to 
  -- overwrite the thunk as the computation keeps on evaluating

main = do
    -- order of evaluation is "inside out"
    -- `repeat' 1` never terminates
    print $ take' 10 $ map' (+1) (repeat' 1)