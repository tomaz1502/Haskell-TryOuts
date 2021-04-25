type InType  = [(Int, Int)]
type OutType = Double
 
sq :: Int -> Double
sq x = xf * xf
    where xf = fromIntegral x

solve :: InType -> OutType
solve [] = 0
solve [_] = 0
solve ((x1, y1) : p2@(x2, y2) : t) = curr_dist + solve (p2 : t)
    where curr_dist = sqrt (sq (abs (x2 - x1)) + sq (abs (y2 - y1)))

deal :: [String] -> [OutType]
deal [] = []
deal (ns : rest) = solve (info ++ [head info]) : deal (drop n rest)
    where n = read ns :: Int
          getPair [a,b] = (a,b)
          inp = take n rest
          info = map (getPair . map (read :: String -> Int) . words) inp


main :: IO ()
main = interact (unlines . map show . deal . lines)
