{-# LANGUAGE InstanceSigs #-}

module Ch23.ChapterExercises where

{- SOME COMMENTS IN THIS SOURCE CODE ARE TAKEN FROM `transformer` SOURCE CODE -}

main :: IO ()
main = do
  putStrLn "✎✎✎✎✎ Functor (NhaNuoc s) ✎✎✎✎✎"
  putStrLn "☘ fmap: `runNhaNuoc ((+1) <$> (NhaNuoc $ \\x -> (0, x))) 0`"
  putStrLn $ show $ runNhaNuoc ((+1) <$> (NhaNuoc $ \x -> (0, x))) 0
  putStrLn "✎✎✎✎✎ Applicative (NhaNuoc s) ✎✎✎✎✎"
  putStrLn "☘ pure: `runNhaNuoc (pure 'X') 1"
  putStrLn $ show $ runNhaNuoc (pure 'X') 1
  putStrLn "✎✎✎✎✎ PRIMITIVES ✎✎✎✎✎"
  putStrLn "☘ get: `runNhaNuoc get \"banhCuonIsAmazing\"`"
  putStrLn $ show $ runNhaNuoc get "banhCuonIsAmazing"
  putStrLn "☘ put: `runNhaNuoc (put \"banhCuon\") \"chaCa\"`"
  putStrLn $ show $ runNhaNuoc (put "banhCuon") "chaCa"
  putStrLn "✎✎✎✎✎ OTHERS ✎✎✎✎✎"
  putStrLn "☘ exec: `exec (put \"banhCuon\") \"chaCa\"`"
  putStrLn $ show $ exec (put "banhCuon") "chaCa"
  putStrLn "☘ eval: `eval get \"banhCuon\"`"
  putStrLn $ show $ eval get "banhCuon"
  putStrLn "☘ modify: `runNhaNuoc (modify (+1) >> modify (*2)) 0`"
  putStrLn $ show $ runNhaNuoc (modify (+1) >> modify (*2)) 0

-- "NhaNuoc" is Vietnamese translation for French's "Moi"
newtype NhaNuoc s a = 
  -- `runNhaNuoc` unwrap the state monad computation as a function
  NhaNuoc { runNhaNuoc :: s -> (a, s) }

instance Functor (NhaNuoc s) where
  fmap :: (a -> b) -> NhaNuoc s a -> NhaNuoc s b
  -- fmap f (NhaNuoc g) = NhaNuoc (\s -> (_0 , _1)); _0 :: b; _1 :: s
  fmap f (NhaNuoc g) =  
    NhaNuoc $ \x0 -> 
      let (v0, x) = g x0  
      in (f v0, x)

instance Applicative (NhaNuoc s) where
  pure :: a -> NhaNuoc s a
  pure v0 = NhaNuoc (\x -> (v0, x))

  (<*>) :: NhaNuoc s (a -> b) -> NhaNuoc s a -> NhaNuoc s b
  NhaNuoc f'' <*> nNg = 
  -- ^ f'' :: s -> ((a -> b), s)  
    -- NhaNuoc $ \x0 -> _0; _0 :: (b, s)
    NhaNuoc $ \x0 -> 
      let 
        (f, x) = f'' x0 -- `f` is the result value, `x` is the updated state
      in runNhaNuoc (f <$> nNg) $ x

instance Monad (NhaNuoc s) where
  return :: a -> NhaNuoc s a
  return = pure

  (>>=) :: NhaNuoc s a -> (a -> NhaNuoc s b) -> NhaNuoc s b
  NhaNuoc f >>= xNg = 
    NhaNuoc $ \x0 -> 
      let 
        (v0, x) = f x0
      in 
        runNhaNuoc (xNg v0) $ x

{- PRIMITIVES: `runNhaNuoc`, `get`, `put` -}

-- | Set the result value but leave the state alone
get :: NhaNuoc s s
-- get = NhaNuoc (liftA2 (,) id id) -- a bit over-engineered
get = NhaNuoc (\x -> (x, x))

-- | Construct a State where the resulting state is the argument provided 
-- | and the value is defaulted to unit
put :: s -> NhaNuoc s ()
put x = NhaNuoc $ const ((), x) 

{- OTHER FUNCTIONS: `exec`, `eval`, `modify` -}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
exec :: NhaNuoc s a  -- ^state-passing computation to execute
     -> s            -- ^initial value
     -> s            -- ^final state
exec m x = snd (runNhaNuoc m $ x)

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
eval :: NhaNuoc s a -- ^state-passing computation to execute
     -> s           -- ^initial state
     -> a           -- ^return value of the state computation
eval m x = fst (runNhaNuoc m $ x)

-- | @'modify' f@ is an action that updates the state to 
-- the result of applying @f@ to the current state.
modify :: (s -> s) -> NhaNuoc s ()
modify f = NhaNuoc $ \x -> ((), f x)