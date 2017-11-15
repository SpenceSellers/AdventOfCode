module Parsing where

import Text.Parsec

sym :: String -> Parsec String () String
sym s = string s <* spaces

parseInt :: Parsec String () Int 
parseInt = read <$> many1 digit

letterNumber :: Parsec String () (Char, Int)
letterNumber = do
    c <- letter
    i <- parseInt 
    return (c, i)

charGrid :: Parsec String () [[Char]]
charGrid = many1 $ (many1 $ noneOf ['\n']) <* endOfLine

-- Parses one of two values, a "true" string and a "false" string
parseBool :: String -> String -> Parsec String () Bool
parseBool true false = (string true  >> return True) 
                   <|> (string false >> return False)
