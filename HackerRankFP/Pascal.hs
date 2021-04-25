fat :: Integer -> Integer
fat 0 = 1
fat n = n * fat (n - 1)

comb :: Integer -> Integer -> Integer
comb n k = fat n `div` (fat k * fat (n - k))

loop :: Integer -> IO ()
loop = go 0 0
    where go i j n
            | j < i      = putStr (show (comb i j) ++ " ") >> go i (j + 1) n
            | i < n - 1  = putStr (show (comb i j) ++ "\n") >> go (i + 1) 0 n
            | otherwise  = putStr (show (comb i j) ++ "\n")

main :: IO ()
main = getLine >>= \ns -> loop (read ns :: Integer)
