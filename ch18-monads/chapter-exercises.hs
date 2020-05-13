import Control.Monad
-- CHAPTER EXERCISE: WRITE INSTANCE 
-- {  check out `./proptest-monads` }

-- CHAPTER EXERCISE: WRITE FUNCTIONS

j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap  

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f     = return []                    
meh (x:xs) f = liftM2 (:) (f x) (meh xs f)  

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id 
-- in here `id` is enough, because when `meh` is called
--    it will lift (:) anyway