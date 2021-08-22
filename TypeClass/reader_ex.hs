import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

monadicTupled :: [Char] -> ([Char], [Char])
monadicTupled = do
    capped <- cap
    reversed <- rev
    return (capped, reversed)

monadicTupled' :: [Char] -> ([Char], [Char])
monadicTupled' =
    cap >>= \capped ->
    rev >>= \reversed ->
    return (capped, reversed)
