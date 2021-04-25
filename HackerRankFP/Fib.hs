fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = go 0 1 2
  where go f1 f2 curr
          | curr == n = f2
          | otherwise = go f2 (f1 + f2) (curr + 1)

main :: IO ()
main = getLine >>= \ns -> print $ fib (read ns :: Int)
