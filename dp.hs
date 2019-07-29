-- Based on http://jelv.is/blog/Lazy-Dynamic-Programming/
-- Edit Distance (Strings) algorithm implemented in Haskell, to exemplify how one
-- can do Dynamic Programming in Functional Programming

import Data.Array

-- First, fibonacci, for warm up:
-- -- The key ideia is to create the look-up table for dp recursively,
-- -- Haskell's lazy evaluation will calculate everything only once.
-- -- We can do it very easily with Lists:

fib :: Int -> Int
fib n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- -- This is fine, but generally, it will work better with Arrays:

fib_arr :: Int -> Int
fib_arr n = dp n
    where dp 0 = 0
          dp 1 = 1
          dp n = fibs ! (n - 1) + fibs ! (n - 2)
          fibs = listArray (0, n) [fib_arr i | i <- [0..n]]

-- Now, we will implement string distance. One can read about the problem and
-- it's solution on the link on the first line. Here we will focus only on it's
-- implementation.

dis :: String -> String -> Int
dis s t = dp ls lt
    where
        dp i 0                 = i
        dp 0 j                 = j
        dp i j
            | (s' ! i) == (t' ! j) = memo ! (i-1, j-1)
            | otherwise        = minimum [memo ! (i-1, j), memo ! (i, j-1), memo ! (i-1, j-1)] + 1
        ls = length s
        lt = length t
        s'                     = listArray (1, ls) s
        t'                     = listArray (1, lt) t
        memo = listArray ((0, 0) , (ls, lt)) [(dp i j) | i <- [0..ls] , j <- [0..lt]]
