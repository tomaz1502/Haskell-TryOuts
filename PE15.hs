import Data.Array

solve :: Int -> Int -> Int
solve i j = dp i j
    where dp x 0 = 1
          dp 0 y = 1
          dp x y = (memo ! (x-1, y)) + (memo ! (x, y-1))
          memo = listArray ((0,0), (i,j)) [ dp x y | x <- [0..i] , y <- [0..j] ]
