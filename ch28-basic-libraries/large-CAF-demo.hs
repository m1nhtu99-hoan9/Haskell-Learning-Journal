module Main where

{-
To produce profiling report: 

```bash
$ # Without `stack`, `GHC` will fail to acknowledge `Prelude`
$ stack ghc -- -prof -fprof-auto -rtsopts -O2 ./large-CAF-demo.hs 
$ # Profiler output will be dumped to `large-CAF-demo.prof`
$ ./large-CAF-demo +RTS -hc -p
$ # To produce postscript file (can be opened with any PDF reader) demonstrates
$ # how much memory the program used in its runtime
$ hp2ps ./large-CAF-demo.hp
```
-}

incdInts :: [Integer]
incdInts = (+1) <$> [1..]

main :: IO ()
main = do
    print $ incdInts !! 1000
    print $ incdInts !! 9001
    print $ incdInts !! 90301
    print $ incdInts !! 903201
    print $ incdInts !! 9005551
    print $ incdInts !! 9959001
