slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

process :: Int -> Int -> [String] -> Int
process di dj ss = sum [1 | (i, s) <- zip [0..] ss,
                        i `mod` dj == 0,
                        s !! ((di * i `div` dj) `mod` w) == '#']
  where w = length (ss !! 0)

main :: IO ()
main = interact (show . product . (\b -> fns <*> pure b) . lines)
  where fns = map (uncurry process) slopes
