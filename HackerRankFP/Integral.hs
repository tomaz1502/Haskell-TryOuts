import Text.Printf (printf)

eval :: [Int] -> [Int] -> Double -> Double
eval [] _  _ = 0
eval _  [] _ = 0
eval (a : as) (b : bs) x = (af * (x ** bf)) + eval as bs x
    where af = fromIntegral a
          bf = fromIntegral b

delta :: Double
delta = 0.001

get :: ([Int] -> [Int] -> Double -> Double) ->
       Double ->
       Double ->
       [Int] ->
       [Int] ->
       Double
get f l r a b
      | l < r     = delta * f a b l + get f (l + delta) r a b
      | otherwise = 0

getArea :: Double -> Double -> [Int] -> [Int] -> Double
getArea = get eval

getVol :: Double -> Double -> [Int] -> [Int] -> Double
getVol = get (\a b x -> eval a b x ^ 2)

solveF :: Double -> Double -> [Int] -> [Int] -> [Double]
solveF l r a b = [getArea l r a b, pi * getVol l r a b]

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r = solveF (fromIntegral l) (fromIntegral r)

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
