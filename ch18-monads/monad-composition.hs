import Control.Monad ((>=>))

mcomp :: Monad m => (b -> m c)
                 -> (a -> m b)
                 -> (a -> m c)
mcomp f g a = (g a) >>= f 
--mcomp f g a = join (f <$> g a)

mcomp' :: Monad m => (a -> m b)
                  -> (b -> m c)
                  -> (a -> m c)
mcomp' f g a = (f >=> g) a