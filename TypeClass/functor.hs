-- Exercises from https://wiki.haskell.org/Typeclassopedia

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

newtype Wot a = X (a -> Int)
instance Functor Wot where
    (<$>) f (X g) = undefined -- this type can't have an instance of Functor!


main :: IO ()
main = undefined
