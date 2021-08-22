{-# LANGUAGE InstanceSigs #-}
import Control.Applicative (liftA2)

hurr = (* 2)
durr = (+ 10)

m :: Integer -> Integer
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

m4 :: Integer -> Integer
m4 = hurr >>= \f ->
     durr >>= \g ->
     return (f + g)

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb


newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader (f . ra)

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader (const a)

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader f) <*> (Reader g) = Reader (\r -> f r (g r))

