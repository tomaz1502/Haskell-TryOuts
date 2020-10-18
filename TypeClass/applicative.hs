import Prelude hiding (Applicative, sequence, (<*>), (*>), (<*), pure)

class Functor f => Applicative f where
    pure :: a -> f a
    infixl 4 <*>, *> -- <*
    (<*>) :: f (a -> b) -> f a -> f b

    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2

    -- (<*) :: f a -> f b -> f a
    -- (<*) = liftA2 const


instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure = Just

    -- <*> :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing  <*> _       = Nothing
    _        <*> Nothing = Nothing
    (Just f) <*> (Just x) = Just (f x)

instance Applicative [] where 
    -- pure :: a -> [a]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | x <- xs , f <- fs]

newtype ZipList a = ZipList { getZipList :: [a] }
    deriving (Eq, Show)

instance Functor ZipList where
    fmap = undefined

instance Applicative ZipList where
    -- pure = a -> ZipList a
    pure x = ZipList [x | _ <- [1 .. ]]

    -- (<*> :: ZipList (a -> b) -> ZipList a -> ZipList b)
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)

instance Applicative ((->) t) where
  -- pure ::
  --   a
  --   -> ((->) t a)
  pure =
    const
  -- (<*>) ::
  --   ((->) t (a -> b))
  --   -> ((->) t a)
  --   -> ((->) t b)
  (<*>) tab ta t = tab t (ta t)

up :: a -> [a]
up x = [x]

upApp :: Applicative f => f a -> f [a]
upApp = (<*>) (pure up)

combine :: Applicative f => f [a] -> f [a] -> f [a]
combine a = (<*>) ((<*>) (pure (++)) a)

sequence :: Applicative f => [f a] -> f [a]
sequence as = foldl combine (pure []) aas
    where aas = map upApp as



