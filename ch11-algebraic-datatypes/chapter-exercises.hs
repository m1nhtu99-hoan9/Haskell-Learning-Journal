module Chap11.Exercises where
  import Data.Char
  import Data.List

  -- @region: VigenereCipher
  type SecretWord = String
  type StringWord = String 

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

  -- @endregion

  -- @region: As-pattern
  indexOf :: (Eq a) => a -> [a] -> Int
  indexOf x ys = case elemIndex x ys of 
    Just a -> a 
    Nothing -> -1

  splitBy :: (Eq a) => a -> [a] -> ([a], [a])
  splitBy x = span (/= x)

  joinWithout :: (Eq a) => a -> [a] -> [a]
  joinWithout x ys = let (a, b) = splitBy x ys in (a ++ tail b)  

  isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
  isSubseqOf [] _ = True 
  isSubseqOf _ [] = False
  -- @TODO: strip each element of the referenced list after each recursion
  isSubseqOf (x : xs) ys 
    | null ys'' = False
    | otherwise = 
        case xs of 
          (z : _) -> 
            let idX = indexOf x ys
                idZ = indexOf z ys in
            case (compare idX idZ) of 
              LT -> isSubseqOf xs (joinWithout x ys)
              _  -> False
          _       -> isSubseqOf xs (joinWithout x ys)
    where (ys', ys'') = splitBy x ys
  
  -- @endregion
