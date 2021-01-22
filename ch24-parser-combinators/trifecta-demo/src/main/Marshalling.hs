{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Data.Aeson ( 
    Value
  , decodeStrict
  , decode 
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import Text.RawString.QQ

main :: IO ()
main = do 
  let blah = decode sampleJson :: Maybe Value
  print blah

sampleJson :: LazyBS.ByteString
sampleJson = [r|
{ "section": { "host": "github.com" }, 
  "whatisit": { "red": "how_are_you_doing?" }
}
|]