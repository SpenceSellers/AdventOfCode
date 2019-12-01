module AdventLib where

import qualified System.Environment
import Data.List.Split as Split
import Data.Maybe
import Data.List
import Safe
import Control.Lens
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy.Char8 as B

firstArg :: IO (Maybe String)
firstArg = headMay <$> System.Environment.getArgs

inputFile :: IO String
inputFile = readFile =<< fromMaybe "input.txt" <$> firstArg

inputLines :: IO [String]
inputLines = lines <$> inputFile

firstStar :: Show a => a -> IO ()
firstStar result = putStrLn $ "First Star: " ++ (show result)

secondStar :: Show a => a -> IO ()
secondStar result = putStrLn $ "Second Star: " ++ (show result)

-- The set of COMPLETE windows across a list
windows :: Int -> [a] -> [[a]]
windows n l@(_:rest)
    | length l < n = []
    | otherwise = take n l : windows n rest

-- use Data.List.Split.chunksOf for chunking

isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == reverse a

indexes :: [a] -> [Int]
indexes a = [0..length a - 1]

-- Rotate a list by N
rotate :: Int -> [a] -> [a]
rotate n a = map nth (indexes a)
    where nth x = a !! ((x - n) `mod` length a)

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

bitshow :: [[Bool]] -> String
bitshow = unlines . map (map bit)
    where bit True  = 'X'
          bit False = '.'

transposed :: Iso [[a]] [[a]] [[a]] [[a]]
transposed = iso transpose transpose

md5 :: String -> String
md5 = show . MD5.md5 . B.pack

fromBool :: Bool -> a -> Maybe a
fromBool True = Just
fromBool False = const Nothing

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

histogram :: Eq a => [a] -> [(a, Int)]
histogram l = hist <$> uniques
    where uniques = nub l
          hist a = (a, count a l)

-- Iterate over a function until we stop getting results.
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f initial = initial : case f initial of
    Just n -> iterateMaybe f n
    Nothing -> []