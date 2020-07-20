isSquare :: Integer -> Bool
isSquare x = (round $ sqrt $ fromInteger x) ^ 2 == x

fatorize :: Integer -> [Integer]
fatorize n = go n 1
    where
        go n x
            | x * x > n      = []
            | n `mod` x == 0 = x : n `div` x : go n (x + 1)
            | otherwise      = go n (x + 1)

-- solve :: Integer -> Maybe Integer
-- solve n = go n [ i * i | i <- [1 .. ] ]


-- 3 -> 1
-- 5 -> 4
-- 7 -> 9
-- 9 -> 16
