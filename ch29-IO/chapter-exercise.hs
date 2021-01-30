module FileIOWithVigenere (
    vignereEncrypt
  , vignereDecrypt
  ) where

import Data.Char (
    toUpper
  , ord
  , chr
  , isAlpha
  )
import Data.List (
    cycle
  , length
  )
import Data.IORef (
    newIORef
  , readIORef
  , writeIORef
  )
import Data.Typeable ( typeOf )
import System.Environment ( getArgs )
import System.Exit ( 
    exitSuccess
  , exitFailure 
  )
import System.IO (
    hGetChar 
  , hPutStr
  , hPrint
  , hWaitForInput 
  , print
  , putStrLn
  , getContents
  , stdout
  , stdin
  , stderr
  )

data ExecMode = Decrypting | Encrypting | None

main :: IO ()
main = do
    args <- getArgs
    -- initiate mutable variable in IO to store current exec mode
    mRef <- newIORef None
    -- initiate mutable variable in IO to store keyword string 
    ksRef <- newIORef ""
    case args of
        ["-d", ks] -> 
            writeIORef mRef Decrypting >> writeIORef ksRef ks 
        ["-e", ks] -> 
            writeIORef mRef Encrypting >> writeIORef ksRef ks
        _ -> printHelp >> exitFailure
    -- read the pointer to get immutable value wrapped inside `ksRef`
    ksRef <- readIORef ksRef -- print . typeOf $ ksRef
    -- read the pointer to get immutable value wrapped inside `mRef`
    mRef <- readIORef mRef -- print . typeOf $ mRef 
    -- wait for input via `stdin` for 15 secs
    done <- hWaitForInput stdin 15000
    if done
    then do
        xsInp <- getContents -- `getContents` is identical to `hGetContents stdin` 
        let 
          xsOut = case mRef of
              Decrypting -> vignereDecrypt ksRef xsInp
              Encrypting -> vignereEncrypt ksRef xsInp
        print xsOut -- `print` is identical to `hPrint stdout`
    else do
        hPrint stderr "Timeout error!"
        exitSuccess
    -- return () -- explicitly

printHelp :: IO ()
printHelp = do
    putStrLn "Vigenère Encryption & Decryption"
    putStrLn mempty
    putStrLn "Usage:"
    putStrLn "\t<mode> <keyword>"
    putStrLn "Modes:"
    putStrLn "\t'-d': Decrypt"
    putStrLn "\t'-e': Encrypt"

{- Vigenère Encryption & Decryption algorithms -}

type OriginalWord = String
type KeyWord = String
type EncryptedString = String

vignereEncrypt :: OriginalWord -> KeyWord -> EncryptedString 
vignereEncrypt xs ks = zipWith go xs' ks'
  where
    xs' = cleanString xs
    ks' = repeatTimes (length xs') (cleanString ks)
    go c1 c2 = shift (ord c1 - 65 + ord c2 - 65) 

vignereDecrypt :: EncryptedString -> KeyWord -> OriginalWord 
vignereDecrypt xs ks = zipWith go xs ks'
  where
    ks' = repeatTimes (length xs) (cleanString ks)
    go c1 c2 = shift (ord c2 - ord c1 - 65) 

repeatTimes :: Int -> String -> String
repeatTimes n xs = take n $ cycle xs  -- `cycle` repeats and concats at the same time

cleanString :: String -> String
cleanString = map toUpper . filter isAlpha

shift :: Int -> Char
-- ord 'A' = 65
shift n = chr (n `mod` 26 + 65)


