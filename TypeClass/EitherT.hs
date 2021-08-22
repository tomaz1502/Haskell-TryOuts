{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT et) = EitherT $ fmap (fmap f) et

instance Applicative m => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure a = undefined

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (<*>) (EitherT f) (EitherT a) = undefined
