module Main where
import AdventLib
import Data.List

type RegionLengthRequirement = Int -> Bool

isValid :: String -> -> RegionLengthRequirement -> Bool
isValid regionLength s = (hasExactlyDouble s) && (neverDecrease 0 $ (digits s))

digits :: String -> [Int]
digits s = read <$> digitStrings
    where digitStrings = (\x -> [x]) <$> s

hasDouble :: Eq a => [a] -> Bool
hasDouble s = any (\[a, b] -> a == b) chunks
    where chunks = windows 2 s

hasExactlyDouble :: Eq a => [a] -> RegionLengthRequirement -> Bool
hasExactlyDouble l req = any req $ length <$> group l

neverDecrease :: Int -> [Int] -> Bool
neverDecrease biggest (x:xs) = if x >= biggest then neverDecrease x xs else False
neverDecrease _ [] = True

main :: IO ()
main = do
    let candidates = [152085..670283];
    firstStar $ length $ filter isValid (==2) (show <$> candidates)
