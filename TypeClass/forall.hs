{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.RWS
import Control.Monad.Reader (runReaderT)

type Combined a = forall (m :: * -> *).
                  ( MonadReader String m
                  , MonadIO m
                  ) => m a

ex :: Combined String
ex = liftIO getLine >>= \s ->
     ask >>= \t ->
     return $ s ++ t

main :: IO ()
main = runReaderT ex "bla" >>= putStrLn

