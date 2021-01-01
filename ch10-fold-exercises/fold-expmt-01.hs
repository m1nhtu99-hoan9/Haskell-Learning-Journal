module Experiment_on_How_Fold_Works where
  
  --the first bit of `spine` must always be evaluated
  doNothing = (\_ _ -> 9001)
  example1 = foldr doNothing 0 ['a'..'z']
  example2 = foldr doNothing 0 ([1..5] ++ undefined)
  example3 = foldr doNothing 0 [undefined, undefined] 
  
  {--
  ```
  example4 = foldr doNothing 0 undefined
  ```
  will not work

  `example3` eveluated because the cons cells contain bottom
  but it self is not a bottom
  --}

  --folding function may not ask for evaluation of the rest of spine
  example5 = foldr const '%' ['a', undefined] 
  example6 = foldr const '!' (['F'..'Z'] ++ undefined)
  example7 = foldr const '!' (undefined : ['D'..'F']) -- will trigger an exception 

  -- the folding function of `foldl` always has first argument as accumulated value
  example8 = foldl (\acc x -> acc + show x) "" [1..25] 

  -- @region: demo differences between `foldr` & `foldl`
  arr = [1..25] ++ undefined
  
  example9 = foldr const 0 arr -- evaluates to 1
  {--
  `example10 = foldr (flip const) 0 arr` evaluates to exception
  `example11 = foldl const 0 arr`        evaluates to exception    
  `example12 = foldl (flip const) 0 arr` evaluates to exception    

  --}
  -- @endregion
