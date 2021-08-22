import Control.Monad.Reader

f :: Int -> String
f x = show x ++ "bla"

g :: Int -> Bool
g x = x /= 0

h :: Int -> Int
h = (+ 10)

f' :: Reader Int String
f' = ask >>= \x -> return $ f x

g' :: Reader Int Bool
g' = ask >>= \x -> return $ g x

h' :: Reader Int Int
h' = ask >>= \x -> return $ h x

z :: Reader Int Int
z = ask >>= \x ->
    ask >>= \y ->
    return (x + (100000 * y))


h'' :: Reader Int Int
h'' = h' >>= \x ->
      ask >>= \y ->
      return (x + (1000000 * y))
