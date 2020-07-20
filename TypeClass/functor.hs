-- Exercises from https://wiki.haskell.org/Typeclassopedia
{-# LANGUAGE FlexibleContexts #-}
import Prelude hiding (Functor, (<$>))

class Functor k where
  (<$>) ::
    (a -> b)
    -> k a
    -> k b

infixl 4 <$>

instance Functor (Either a) where
    (<$>) _ (Left a)  = Left a
    (<$>) f (Right b) = Right (f b)

instance Functor ((->) e) where
    (<$>) = (.)

data Pair a = Pair a a deriving Show

instance Functor ((,) e) where
    (<$>) f (a, b) = (a, f b)

instance Functor Pair where
    (<$>) f (Pair x y) = Pair (f x) (f y)

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
    (<$>) f (Leaf g)  = Leaf (f . g)
    (<$>) f (Node xs) = Node (map (f <$>) xs)

newtype Wot c a = X (a -> c)
instance Functor (Wot c) where
    (<$>) f (X g) = undefined -- this type can't have an instance of Functor!


-- the composition of functors is a functor:
newtype Compose f g a = Compose { getCompose :: f (g a) }
instance (Functor f, Functor g) => Functor (Compose f g) where
    (<$>) h (Compose x) = Compose ((h <$>) <$> x)


main :: IO ()
main = undefined
