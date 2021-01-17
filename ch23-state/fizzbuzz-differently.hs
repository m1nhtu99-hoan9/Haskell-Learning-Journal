import Data.List ( intersperse )
import Data.Foldable ( concat, mapM_ )
import Control.Monad.Trans.State.Lazy ( State, execState, get, put )

main :: IO ()
main = do
  putStrLn "✎✎✎✎✎✎✎✎✎✎✎✎✎✎✎ `fizzBuzzList` ✎✎✎✎✎✎✎✎✎✎✎✎✎✎✎"
  putStrLn . concat . intersperse "; " $ fizzBuzzList [1..100]
  putStrLn "✎✎✎✎✎✎✎ Exercise: FizzBuzz Differently ✎✎✎✎✎✎✎"
  putStrLn . concat . intersperse "; " $ fizzBuzzFromTo 1 100 

fizzBuzzFromTo :: Int -> Int -> [String]
fizzBuzzFromTo x y = 
  case compare x y of 
    GT -> error "First argument need to be smaller than or equal to the second one"
          -- ^ very dirty trick, not recommended in production code
    EQ -> fizzBuzzList [y]
    LT -> fizzBuzzList [y,y-1..x]                        

fizzBuzzList :: [Int] -> [String]
fizzBuzzList ls = execState (mapM_ addResult ls) []
                -- ^ execState s a -> s -> s; s ~ [String]
                -- Evaluate state computation with given initial state value (in this case: []) 
                -- and return the final state
                          -- ^ mapM :: (Foldable t, Monad m) => (a -> m b) -- monadic action
                          --                                 -> t a        -- foldable structure 
                          --                                 -> m ()       -- the result is ignored
                          --            a ~ Int; t ~ []; m ~ State [String]
                      
  where
    addResult :: Int -> State [String] ()
                         -- ^ `State` is a type alias for `StateT`
    addResult n = do
        xs <- get
           -- ^ get :: Monad m => StateT s m s -- get the current value of the state
        let x = fizzBuzz n -- "let-statement", not to be confused with "let-expression"
        put (x : xs) -- put the state within the State monad to (x : xs) 
        -- ^ using `cons` operator means that we're evaluating 
        -- the returned singly-linked list in reverse

fizzBuzz :: Int -> String
fizzBuzz n = 
  let
    -- s = liftA2 (<>) fizz buzz $ n -- a bit over-engineered
    s = fizz n <> buzz n
  in
    case null s of 
      True -> show n
      False -> s

fizz :: Int -> String
fizz n  
  | (n `mod` 3 == 0) = "Fizz" 
  | otherwise = ""

buzz :: Int -> String
buzz n  
  | (n `mod` 5 == 0) = "Buzz"
  | otherwise = ""



