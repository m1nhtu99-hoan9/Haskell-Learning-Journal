module Chap11.Exercises where
  import Data.Char
  import Data.List

  {-- ord 'A' == 65
      ord 'Z' == 90
    ord {the first showable char, ' '} == 32
    ord {the last showable char, '~'} == 126
  --}

  -- ord :: Char -> Int; chr :: Int -> Char

  upperWord :: String -> String 
  upperWord = map toUpper 

  shiftUpperChar :: Int -> Char -> Char 
  shiftUpperChar n a = let ordNewChar = (ord (toUpper a) + n) in chr ((+65) (mod (ordNewChar - 65) 26)) 
  
  type SecretWord = String
  type StringWord = String 

  assimilateByLength :: StringWord -> SecretWord -> StringWord
  assimilateByLength x = take (length x) . cycle

  charToShiftNum :: Char -> Int
  charToShiftNum = (subtract 65) . ord . toUpper

  reduceToDecryptedChar :: StringWord -> Char
  reduceToDecryptedChar x = shiftUpperChar (charToShiftNum (last x)) (head x)

  implementVigenereCipherOnWord :: StringWord -> SecretWord -> StringWord
  implementVigenereCipherOnWord x y = let assimilatedY = (assimilateByLength x y) in 
    (map (reduceToDecryptedChar) (transpose [x, assimilatedY])) 
                                     
  implementVigenereCipher :: SecretWord -> String -> String
  implementVigenereCipher x y = unwords (map (flip implementVigenereCipherOnWord x) (words y))

