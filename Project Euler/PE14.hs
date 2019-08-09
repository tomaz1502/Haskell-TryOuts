import Data.Array

maxIdH :: Int -> Int -> Int -> Array Int Int -> Int
maxIdH ret c i arr
    | i > e        = ret
    | c < (arr ! i) = maxIdH i (arr ! i) (i+1) arr
    | otherwise     = maxIdH ret c (i+1) arr
    where (_, e) = bounds arr

maxId :: Array Int Int -> Int
maxId v = maxIdH 1 (-1) 1 v

collatz_MAX :: Int
collatz_MAX = maxId memo
    where
        solve x 
            | x == 1         = 1
            | y > lim        = 1 + (solve y)
            | otherwise      = 1 + (memo ! y)
            where
                y = if (x `mod` 2  == 1) then (3 * x + 1) else (x `div` 2)
        memo = listArray (1, lim) [ solve z | z <- [1..lim] ]
        lim = 10^6
