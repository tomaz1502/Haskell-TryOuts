import Prelude hiding ((-), (^))
import qualified Prelude as P

type Point = (Double, Double)
type InType  = [Point]
type OutType = Double

-- cross product
(^) :: Point -> Point -> Double
(x1, y1) ^ (x2, y2) = (x1 * y2) P.- (y1 * x2) 

(-) :: Point -> Point -> Point
(x1, y1) - (x2, y2) = (x1 P.- x2, y1 P.- y2)

trgArea :: Point -> Point -> Point -> Double
trgArea p1 p2 p3 = ((p2 - p1) ^ (p3 - p2)) / 2

solve :: InType -> OutType
solve [] = 0
solve [_] = 0
solve (p1@(x1, y1) : p2@(x2, y2) : t) = trgArea (0,0) p1 p2 + solve (p2 : t)

deal :: [String] -> [OutType]
deal [] = []
deal (ns : rest) = abs (solve (info ++ [head info])) : deal (drop n rest)
    where n = read ns :: Int
          getPair [a,b] = (a, b)
          inp = take n rest
          info = map (getPair . map (read :: String -> Double) . words) inp


main :: IO ()
main = interact (unlines . map show . deal . lines)
