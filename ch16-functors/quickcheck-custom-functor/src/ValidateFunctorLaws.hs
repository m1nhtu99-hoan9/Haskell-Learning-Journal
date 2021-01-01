module ValidateFunctorLaws where 

import Test.QuickCheck
import Test.QuickCheck.Function

-- f is any data constructor that has an `Functor` instance
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposability :: (Functor f, Eq (f c)) =>
                  (a -> b)
               -> (b -> c)
               -> f a 
               -> Bool
functorComposability f g i = 
  (fmap g (fmap f i)) == (fmap (g . f) i) 

-- To test `functorComposability`, we need to pass concrete values in; 
-- for instance, `quickCheck (functorComposability (+1) (*2) (x :: Int))`

-- However, QuickCheck can generate functions as well
functorComposability' :: (Eq (f c), Functor f) => 
                   f a
                -> Fun a b
                -> Fun b c 
                -> Bool
functorComposability' i (Fun _ f) (Fun _ g) = 
  (fmap (g . f) i) == (fmap g . fmap f $ i)
-- a caveat: `Fun` values can't be printed

listIntToBoolAssoc :: [Int] -> Bool
listIntToBoolAssoc x = functorIdentity x

type FunIntToInt = Fun Int Int 
type IntToIntAssoc = [Int] -> FunIntToInt -> FunIntToInt -> Bool