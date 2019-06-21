f :: Integer -> Integer -> Integer
f a b
  | a <= 2*b  = 3*(a-b)
  | a <= 3*b  = 3*(a-b) + 2*(a-2*b)
  | otherwise = 4*(a-b) + (f (a - 2*b) b)

main = do
  ln <- getLine
  let [a, b] = map read (words ln)
  print (f a b)
