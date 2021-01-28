import Debug.Trace
import Data.Either
import Text.Parsec
import Text.Parsec.String

data Field = MkField String String
    deriving Show

newtype Passport = MkPassport [Field]
    deriving Show

field :: Parser Field
field = do key  <- count 3 $ oneOf ['a'..'z']
           char ':'
           value <- many1 $ noneOf " \n"
           return (MkField key value)

passport :: Parser Passport
passport = MkPassport <$> ((:) <$> field <*> many (try $ oneOf " \n" *> field))

passports :: Parser [Passport] 
passports = (:) <$> passport <*> passports2

passports2 :: Parser [Passport]
passports2 = try (count 2 (char '\n') *> passports) <|> pure []

validPassport :: Passport -> Bool
validPassport (MkPassport fs) = all (\field -> field `elem` keys fs) requiredFields
    where keys = map (\(MkField key value) -> key)
          requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

process str = fromRight 0 $ length . filter validPassport <$> parse passports "" str

main = interact $ show . process
