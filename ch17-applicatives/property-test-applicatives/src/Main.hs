module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import CustomTypes

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  --quickBatch (applicative trg)
  quickBatch (applicative trgSum)
  quickBatch (monad trgSum)

-- Monoid laws as QuickCheck properties has already been bundled
--  into TestBatch called `monoid`.  
-- A concrete value passed into `monoid` so that this function can
--  recognise which Arbitrary instance to use for generating random values.
-- Similarly, even though `trg` is a bottom value in this case, 
--  the purpose of its being passed into `applicative` function
--  is that it will inform `applicative` to generate values for which types.