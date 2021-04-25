type InType  = [(Int, Int)]
type OutType = Bool
 
solve :: InType -> OutType
solve = go []
    where go ps [] = True
          go ps (p : xs) = all (check p) ps && go (p : ps) xs
          check (x1, y1) (x2, y2) = x1 /= x2 || y1 == y2
 
receive :: [String] -> InType
receive s = undefined
 
showb :: Bool -> String
showb True  = "YES"
showb False = "NO"

deal :: [String] -> [Bool]
deal [] = []
deal (ns : rest) = solve info : deal (drop n rest)
    where n = read ns :: Int
          getPair [a,b] = (a,b)
          inp = take n rest
          info = map (getPair . map (read :: String -> Int) . words) inp


main :: IO ()
main = interact
  (unlines . map showb . deal . tail . lines)
