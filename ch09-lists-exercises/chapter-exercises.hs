module Chap09.Exercises where
import Data.Char

onlyCaps :: String -> String 
onlyCaps "" = error "We don\'t do empty string here!!!"
onlyCaps x  = filter isUpper x

capitalise :: String -> String
capitalise "" = error "-We- don\'t do empty string here!!!"
capitalise x  = concat [(map toUpper a),b] 
                  where (a,b) = splitAt 1 x

capitalise' :: String -> String
capitalise' "" = error "-We- don\'t do empty string here!!!"
capitalise' x  = concat [[toUpper a], b]
                    where (a, b) =  (head x, tail x)

{-- ord 'a' == 97
    ord 'z' == 122
    ord {the first showable char, ' '} == 32
    ord {the last showable char, '~'} == 126
--}

shiftLowerChar :: Int -> Char -> Char 
shiftLowerChar n a = let ordNewChar = (ord a + n) in chr ((+97) (mod (ordNewChar - 97) 26)) 

shiftShowableChar :: Int -> Char -> Char 
shiftShowableChar n a = let ordNewChar = (ord a + n) in chr ((+32) (mod (ordNewChar - 32) 95))

shiftChar :: Int -> Char -> Char 
shiftChar n a = let lowerBound = (compare (ord a) 97)
                    upperBound = (compare (ord a) 126)
                  in case (and [not (upperBound == LT), not (lowerBound == GT)]) of
                    True  -> shiftLowerChar n a
                    False -> shiftShowableChar n a
 
ceasarCipher :: String -> Int -> String 
ceasarCipher "" _ = error "We don\'t do empty string here!!!"
ceasarCipher x n  = map alterChar x where alterChar = shiftChar n 