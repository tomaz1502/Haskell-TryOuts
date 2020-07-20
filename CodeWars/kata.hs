import Data.Char

genUnique :: String -> String -> String -> String
genUnique ans have [] = ans
genUnique ans have (c : cs)
    | elem c have && not (elem c ans) = genUnique (c : ans) have cs
    | not $ elem c have               = genUnique ans (c : have) cs
    | otherwise                       = genUnique ans have cs

duplicateCount :: String -> Int
duplicateCount s = length $ genUnique "" "" (map toLower s)
