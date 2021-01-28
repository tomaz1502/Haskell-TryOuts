import Control.Monad

process :: [Integer] -> [Integer]
process lst = do
  x <- lst
  y <- lst
  z <- lst
  guard $ x + y + z == 2020
  return $ x * y * z
        
main = interact (show . process . map read . lines)
