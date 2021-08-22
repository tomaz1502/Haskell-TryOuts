{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

import Control.Monad.RWS

type ListIO a = forall (m :: * -> *).
                       (MonadIO m
                       ) => m a

main :: ListIO ()
main = runListT [1,2,3] >>=  liftIO print
