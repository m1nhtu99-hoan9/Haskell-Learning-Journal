{-# LANGUAGE InstanceSigs #-}

{- SOME COMMENTS IN THIS SOURCE CODE ARE TAKEN FROM `transformer` SOURCE CODE -}

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runState :: State s a   -- ^state-passing computation to execute
         -> s           -- ^initial state
         -> (a, s)      -- ^return value and final state
runState m = runIdentity . runStateT m

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
evalState :: State s a -- ^state-passing  computation to execute
          -> s         -- ^initial value
          -> a         -- ^return value of state computation
evalState m s = fst (runState m s)

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
execState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> s          -- ^final state
execState m s = snd (runState m s)
