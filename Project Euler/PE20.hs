fat :: Integer -> Integer
fat x
    | x == 0     = 1
    | otherwise  = x * fat (x-1)

log10 :: Integer -> Integer
log10 x
      | x < 10 = 0
      | otherwise = 1 + log10 (x `div` 10)

split :: Integer -> [Integer]
split x
      | x < 10     = [x]
      | otherwise  = digit : (split (x - order * digit))
      where
        lg = log10 x
        order = 10 ^ lg
        digit = x `div` order

main :: IO()
main = do
    inp <- getLine
    let n = read inp :: Integer
    print (sum (split (fat n)))
    
