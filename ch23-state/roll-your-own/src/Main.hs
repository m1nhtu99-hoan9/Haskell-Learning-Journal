module Main (
    Dice
  , rollsToGetN
  , rollsCountLogged
  , rollDice'
  , rollReplicateDices
  , rollsToGetTwentyWithRandomSeedValue
  , main
) where

import Control.Applicative ( liftA3 )
import Control.Monad ( replicateM )
import Control.Monad.Trans.State ( evalState, state, State )

import System.Random (
    StdGen
  , mkStdGen
  -- , next
  -- , random
  , randomIO
  , randomR
  )

main :: IO ()
main = do
  putStrLn "Roll the dices!!!"
  putStrLn $ show $ demo 0
  putStrLn $ show $ demo 1
  putStrLn $ show $ demoDuplicateDices 6 0
  --  ^ [DiceSix,DiceSix,DiceSix,DiceSix,DiceSix,DiceSix]
  putStrLn $ show $ demoDuplicateDices 9 1
  --  ^ [DiceSix,DiceSix,DiceSix,DiceSix,DiceSix,DiceSix,DiceSix,DiceSix,DiceSix]
  putStrLn $ show $ demoReplicateDices 6 0
  --  ^ [DiceSix,DiceSix,DiceFour,DiceOne,DiceFive,DiceTwo]
  putStrLn $ show $ demoReplicateDices 9 1
  --  ^ [DiceSix,DiceFive,DiceTwo,DiceSix,DiceFive,DiceTwo,DiceThree,DiceTwo,DiceFive]
  putStrLn "∞∞∞∞ Exercises: Roll Your Own ∞∞∞∞"
  putStrLn "⚝ Num of rolls to get 30 with seed 0"
  putStrLn $ show $ rollsToGetN 30 (mkStdGen 0)
  putStrLn "⚝ Num of rolls to get 30 with seed 1, logs showed"
  putStrLn $ show $ rollsCountLogged 30 (mkStdGen 1)

data Dice = 
    DiceOne
  | DiceTwo
  | DiceThree
  | DiceFour
  | DiceFive
  | DiceSix
  | DiceNone
  deriving (Eq, Show)

intToDice :: Int -> Dice
intToDice n =
  case n of 
    1 -> DiceOne
    2 -> DiceTwo
    3 -> DiceThree
    4 -> DiceFour
    5 -> DiceFive
    6 -> DiceSix
    x -> error ("intToDice got non 1-6 integer: " <> show x)

type Seed = Int

demo :: Seed -> (Dice, Dice, Dice)
demo n = evalState rollDiceThreeTimes' (mkStdGen n)
--      ^ evalState :: State s a s; s ~ StdGen; a ~ (Dice, Dice, Dice) 

demoDuplicateDices :: Int -> Seed -> [Dice]
demoReplicateDices :: Int -> Seed -> [Dice]
demoDuplicateDices i d = take i $ evalState rollDuplicateDices . mkStdGen $ d
demoReplicateDices i d = evalState (rollReplicateDices i) (mkStdGen d)

-- | `rollDice` refactored
rollDice' :: State StdGen Dice
rollDice' = intToDice <$> state (randomR (1, 6))
            -- `State StdGen` has a final type argument of `Int`
            -- hence, `intToDice` lift `Int -> Dice` over the final type argument
            -- and transform that type argument to `Dice`

-- | `rollDiceThreeTimes` refactored with `State`
rollDiceThreeTimes' :: State StdGen (Dice, Dice, Dice)
rollDiceThreeTimes' = liftA3 (,,) rollDice' rollDice' rollDice'

-- | Repeat a single evaluated `Dice` value
rollDuplicateDices :: State StdGen [Dice]
rollDuplicateDices = repeat <$> rollDice'

-- | Repeat the `State` action that produces the desired `Dice` a number of times
rollReplicateDices :: Seed -> State StdGen [Dice]
rollReplicateDices = flip replicateM rollDice'

-- | Generate three dices, non-optimal version
rollDiceThreeTimes :: (Dice, Dice, Dice)
rollDiceThreeTimes = do
  let 
    s = mkStdGen 0
    (d1, s1) = randomR (1, 6) s
    (d2, s2) = randomR (1, 6) s1
    (d3, _) = randomR (1, 6) s2
  (intToDice d1, intToDice d2, intToDice d3)

-- | Generate a single dice
rollDice :: State StdGen Dice
rollDice = state $ do
  --  ^ `state` function is a constructor that take a `State`-like function
  --     and embeds in the `State` monad transformer
  (n, s) <- randomR (1, 6)
  --        ^ randomR (1, 6) :: (Random a, RandomGen g, Num a) => g -> (a, g)
  return (intToDice n, s)

{- KEEP ON ROLLING -}

-- | Roll a single dice until reaching or exceeding a sum of 20
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    -- `s` for accumulated sum, `i` for tracking count, `g` for gen
    go s i g
      | s >= 20 = i
      | otherwise =
        let (d, g') = randomR (1, 6) g
        in go (s + d) (i + 1) g'

-- | `rollsToGetTwenty` but use `IO` to get random seed value
rollsToGetTwentyWithRandomSeedValue :: IO Int
rollsToGetTwentyWithRandomSeedValue = rollsToGetTwenty . mkStdGen <$> randomIO 

{- EXERCISES: ROLL YOUR OWN -}

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where 
    go :: Int -> Int -> StdGen -> Int
    -- `s` for accumulated sum, `i` for accumulated count, `g` for generator
    go s i g
      | s >= n = i
      | otherwise =
        let (v, g') = randomR (1, 6) g
        in go (s + v) (i + 1) g'

rollsCountLogged :: Int -> StdGen -> (Int, [Dice])
rollsCountLogged n = go 0 0 []
  where 
    go :: Int -> Int -> [Dice] -> StdGen -> (Int, [Dice])
    -- `s` for accumulated sum, `i` for accumulated count, 
    -- `ds` for accumulated list of `Dice`s, `g` for generator 
    go s i ds g 
      | s >= n = (i , ds)
      | otherwise =
        let (v, g') = randomR (1, 6) g
        in go (s + v) (i + 1) (ds ++ [intToDice v]) g'