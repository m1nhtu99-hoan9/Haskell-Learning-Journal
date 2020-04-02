module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO) 

type WordList = [String]
minWordLength = 5 :: Int
maxWordLength = 9 :: Int 

isInGameLength :: String -> Bool 
isInGameLength w = and [length w >= minWordLength, length w < maxWordLength]

-- Puzzle {word for guessing} {[Char] filled in so far} {[Char] guessed so far}
data Puzzle = Puzzle String [Maybe Char] [Char]

getString :: Puzzle -> String 
getString (Puzzle x _ _) = x

getGuessedList :: Puzzle -> [Char]
getGuessedList (Puzzle _ _ xs) = xs

instance Show Puzzle where 
  show (Puzzle _ discovered guessed) = 
    concat [
      intersperse ' ' (fmap renderPuzzleChar discovered), 
      " Guessed so far: ", 
      guessed
    ]

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar c = case isJust c of 
  True -> innerC where (Just innerC) = c
  False -> '_'

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (const Nothing) xs) []

charInWord' :: Char -> Puzzle -> Bool
charInWord' c = elem c . getString

charInWord :: Puzzle -> Char -> Bool
charInWord = flip charInWord'

alreadyGuessed' :: Char -> Puzzle -> Bool
alreadyGuessed' c = elem c . getGuessedList

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed = flip alreadyGuessed'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle w filled xs) c = Puzzle w newFilled (c : xs)
  where 
    zipper guessedChar wordChar guessChars = 
      if wordChar == guessedChar
      then Just wordChar 
      else guessChars 
    newFilled = zipWith (zipper c) w filled

-- @region: side effects

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  allWs <- allWords
  return (filter isInGameLength allWs)

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1) 
  return (wl !! randomIndex)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
  putStrLn ("Your guess was " ++ [guess])
  case (charInWord puzzle guess, 
        alreadyGuessed puzzle guess) of 
    (_, True) -> do
      putStrLn "You already guessed that\
                \ character, pick \
                \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
                \ word, filling in the word\
                \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle w _ guessed) = 
  if (length guessed) > 7 
  then 
    do putStrLn "You lose :("
       putStrLn (concat ["The word was: ", w])
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filled _) = 
  if all isJust filled 
  then 
    do putStrLn "You win!!!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame p = forever $ do
  gameOver p 
  gameWin p
  putStrLn "" 
  putStrLn (concat ["Current puzzle is: ", show p])
  putStrLn "Have a guess: "
  g <- getLine
  case g of 
    [c] -> handleGuess p c >>= runGame
    _   -> putStrLn "Your guess must \
                    \ be a single letter" 

main :: IO ()
main = do
  word <- randomWord' 
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

-- @endregion 