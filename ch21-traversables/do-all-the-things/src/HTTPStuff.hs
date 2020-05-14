module HTTPStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "https://github.com/mnhthng-thms/internal-training-academy"
       , "https://github.com/mnhthng-thms/quadratic-function-solver" ]

mappingGET :: [IO (Response ByteString)]
mappingGET = map get urls

traversedURLs :: IO [Response ByteString]
traversedURLs = traverse get urls