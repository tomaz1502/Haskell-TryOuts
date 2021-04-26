solve :: String -> String -> String
solve [] [] = []
solve (x : xs) (y : ys) = x : y : solve xs ys

main :: IO ()
main =
    getLine >>= \s ->
    getLine >>= \t ->
    putStrLn $ solve s t
