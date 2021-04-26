solve :: String -> String
solve [] = []
solve (x : y : xs) = y : x : solve xs 

main :: IO ()
main = interact (unlines . map solve . tail . lines)
