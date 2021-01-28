import Data.Bits
import Data.Either
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String

parser :: Parser (Int, Int, Char, String)
parser = (,,,)
    <$> (f <$> many1 digit)
    <*  char '-'
    <*> (f <$> many1 digit)
    <*  char ' '
    <*> oneOf ['a'..'z']
    <*  char ':'
    <*  char ' '
    <*> many1 anyChar

f :: String -> Int
f = foldl' (\y x -> 10 * y + (ord x - ord '0')) 0

cnt :: Char -> String -> Int
cnt x = length . filter (x==)

verify :: (Int, Int, Char, String) -> Bool
verify (lower, upper, c, str) = p1 `xor` p2
    where p1 = str !! (lower - 1) == c
          p2 = str !! (upper - 1) == c

process = length . filter (fromRight False . (verify <$>))  . map (parse parser "") . lines

main = interact $ show . process
