module Parsing where

import Text.Parsec
import Text.Parsec.String

sym :: String -> Parser String
sym s = string s <* spaces

constSym :: String -> a -> Parser a
constSym s a = sym s >> return a

parseInt :: Parser Int 
parseInt = parseIntNegative <|> (read <$> many1 digit)

parseIntNegative = (negate . read) <$> (char '-' >> many1 digit)

-- Parses something like A1 or B3
letterNumber :: Parser (Char, Int)
letterNumber = do
    c <- letter
    i <- parseInt
    return (c, i)

bracketed :: String -> Parser a -> Parser a
bracketed (start:stop:_) p = Text.Parsec.between (string [start]) (string [stop]) p

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepBy p (sym ",")

charGrid :: Parser [[Char]]
charGrid = many1 $ (many1 $ noneOf ['\n']) <* endOfLine

-- Parses one of two values, a "true" string and a "false" string
parseBool :: String -> String -> Parser Bool
parseBool true false = (string true  >> return True) 
                   <|> (string false >> return False)
