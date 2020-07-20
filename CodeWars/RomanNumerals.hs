module RomanNumerals where

vals :: [(Int, String)]
vals = zip  [1000, 900, 500,  400, 100,   90,  50,    40,  10,   9,    5,    4,  1]
            ["M" , "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

solution :: Int -> String
solution 0 = ""
solution x = h ++ solution (x - y)
  where (y , h) = head $ filter (\z -> (fst z) <= x) vals
