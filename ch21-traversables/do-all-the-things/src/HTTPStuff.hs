module HTTPStuff where

import Data.ByteString.Lazy (ByteString)
import Network.Wreq (Response, get)

urls :: [String]
urls =
  [ "https://github.com/mnhthng-thms/internal-training-academy",
    "https://github.com/mnhthng-thms/quadratic-function-solver"
  ]

mappingGET :: [IO (Response ByteString)]
mappingGET = map get urls

-- map each `urls` list element to HTTP GET action
-- evaluate these actions from left to right, and collect the results
traversedURLs :: IO [Response ByteString]
traversedURLs = traverse get urls