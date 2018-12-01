module Main where

import AdventLib
import Parsing
import Text.Parsec
import Control.Lens
import Data.List
import Data.Foldable
import Control.Applicative ((<$))

-- IP (Stuff outside the brackets) (Stuff inside the brackets)
data IP = IP [String] [String]
    deriving Show

parseIP :: Parsec String () IP
parseIP = uncurry IP <$> parseIP'

parseIP' :: Parsec String () ([String], [String])
parseIP' = ([], []) <$ eof <|> do
    outStart <- many letter
    inStart <- option Nothing $ Just <$> bracketed "[]" (many letter)
    (outside, inside) <- parseIP'
    return (outStart : outside, toList inStart ++ inside)

hasAbba :: String -> Bool
hasAbba = any isAbba . windows 4
    where isAbba (a:b:c:d:[]) = a == d && b == c && a /= b

supportsTLS :: IP -> Bool
supportsTLS (IP outside inside) = any hasAbba outside && (not . any hasAbba) inside

abas :: String -> [String]
abas = filter isAba . windows 3
    where isAba (a:b:c:[]) = a == c && a /= b

supportsSSL :: IP -> Bool
supportsSSL (IP outside inside) = not . null . intersect outsides $ rev <$> insides
    where outsides = abas =<< outside
          insides = abas =<< inside
          rev (a:b:_) = [b, a, b]

main :: IO ()
main = do
    lines <- inputLines
    let Right ips = sequence $ parse parseIP "" <$> lines
    let numSupporters = length . flip filter ips
    print . numSupporters $ supportsTLS
    print . numSupporters $ supportsSSL
