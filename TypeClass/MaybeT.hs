{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Cont (lift, MonadIO (liftIO))
newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

newtype Compose f g a =
    Compose { runCompose :: f (g a) }
    deriving (Eq, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) =
        Compose $ fmap (fmap f) fga

instance Applicative Identity where
    pure :: a -> Identity a
    pure a = Identity a

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a) = Identity (f a)


lift2 ::
  Applicative k =>
  (a -> b -> c)
  -> k a
  -> k b
  -> k c
lift2 f ka kb = f <$> ka <*> kb

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure (pure a)

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (<*>) (Compose fgAtoB) (Compose fgA) = Compose $ lift2 (<*>) fgAtoB fgA


newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (f <$> fa)

instance Applicative m => Applicative (IdentityT m) where
    pure a = IdentityT (pure a)
    (<*>) (IdentityT fab) (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad m => Monad (IdentityT m) where
    return = pure
    (>>=) (IdentityT ma) f = IdentityT $ ma >>= \a -> runIdentityT (f a)


newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma

instance Applicative m => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure a = MaybeT $ pure (Just a)

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (MaybeT mf) <*> (MaybeT ma) = MaybeT $ lift2 (<*>) mf ma


instance Monad m => Monad (MaybeT m) where
    return :: a -> MaybeT m a
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f = MaybeT $ ma >>= \case
                                           Nothing  -> return Nothing
                                           (Just a) -> runMaybeT (f a)

x :: [Int]
x = [1,2,3,4]

y :: Maybe Int
y = Just 3

myLift :: (Monad m) => Maybe b -> MaybeT m b
myLift = MaybeT . return

myOtherLift :: [a] -> MaybeT [] a
myOtherLift = MaybeT . fmap return

run :: MaybeT [] Int
run = myLift y >>= \y0 ->
      myOtherLift x >>= \x0 ->
      return x0

