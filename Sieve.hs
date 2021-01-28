minus :: [Int] -> [Int] -> [Int]
minus (x:xs) (y:ys) = case compare x y of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
 
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys) = case compare x y of 
           LT -> x : merge  xs  (y:ys)
           EQ -> x : merge  xs     ys 
           GT -> y : merge (x:xs)  ys
merge  xs     []    = xs
merge  []     ys    = ys
 
 
mergeAll :: [[Int]] -> [Int]
mergeAll = foldr (\(x : xs) -> (x : ) . merge xs) []
 
primes :: [Int]
primes = 2 : 3 : minus [5,7..] (mergeAll [[p*p, p*p+2*p..] | p <- tail primes])

main :: IO ()
main = getLine >>= print . (`take` primes) . (read :: String -> Int)
