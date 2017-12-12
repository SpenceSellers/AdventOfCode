module Main where

import Text.Parsec
import Text.Parsec.String
import Parsing
import AdventLib

parseGarbage :: Parser Int
parseGarbage = char '<' >> sum <$> manyTill garbageChar (char '>')
    where garbageChar = (char '!' >> anyChar >> return 0) <|> (anyChar >> return 1)

parseGroup :: Int -> Parser Int
parseGroup level = do
    groups <- buildGroupParser (parseGroup (level + 1)) (parseGarbage >> return 0)
    return $ sum groups + level

buildGroupParser :: Parser a -> Parser a -> Parser [a]
buildGroupParser group garbage = bracketed "{}" $ (group <|> garbage) `sepBy` (sym ",")

parseGroupGarbage :: Parser Int
parseGroupGarbage = sum <$> buildGroupParser parseGroupGarbage parseGarbage

main :: IO ()
main = do
    input <- inputFile
    print $ parse (parseGroup 1 <* eof) "" input
    print $ parse (parseGroupGarbage <* eof) "" input