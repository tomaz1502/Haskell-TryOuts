import Data.List

process :: [String] -> Int
process ss = sum [1 | (i, s) <- zip [0..] ss, s !! (3*i `mod` (length ss !! 0)) == '#']

main :: IO ()
main = interact (show . process . lines)
