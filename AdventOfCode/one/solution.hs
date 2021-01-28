import System.IO (isEOF)

toInt :: String -> Int
toInt s = read s :: Int

solve :: [Int] -> [Int] -> Int
solve []       _    = error "impossible"
solve (x : xs) pref
  | (2020 - x) `elem` pref = x * (2020 - x)
  | otherwise              = solve xs (x : pref)

loop :: [Int] -> IO ()
loop xs = isEOF >>= \done ->
             if done
             then print $ solve xs []
             else getLine >>= \inp -> loop (toInt inp : xs)

main :: IO ()
main = loop []
