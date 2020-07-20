module Parentheses where

helper :: String -> Int -> Bool
helper [] x = x == 0
helper (c : cs) x
    | x < 0      = False
    | c == '('   = helper cs (x + 1)
    | otherwise  = helper cs (x - 1)


validParentheses :: String -> Bool
validParentheses s = helper s 0
