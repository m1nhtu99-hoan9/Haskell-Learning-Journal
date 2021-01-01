import Data.Char
import Control.Applicative (liftA2)

composed :: [Char] -> [Char]
composed = (map toUpper) . reverse

fmapped :: [Char] -> [Char]
fmapped = fmap reverse (map toUpper)

tupled :: String -> (String, String)
tupled = liftA2 (,) (map toUpper) (reverse)
-- it's kinda abstract to think about it at first
-- but just keep in mind that these functions both have kind
-- * -> * and abide to Monoid laws. 

tupledBind :: String -> (String, String)
tupledBind = (map toUpper) <$> reverse >>= (,)




