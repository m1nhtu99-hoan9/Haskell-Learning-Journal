import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

type Embedded = 
    MaybeT 
        (ExceptT 
            String 
            (ReaderT () IO))
        Int

embedded :: Embedded
embedded = MaybeT . ExceptT . ReaderT $ return <$> ms0
  where 
    ms0 :: () -> Either a (Maybe Int)
    ms0 = (const . Right . Just $ 9)

unwrap :: Embedded -> IO (Either String (Maybe Int))
unwrap = evalReaderT . runExceptT . runMaybeT
  where
    evalReaderT :: ReaderT () IO a -> IO a
    evalReaderT = flip runReaderT ()

-- it's awkward to have `main` type signature elaborated here
main = unwrap embedded
