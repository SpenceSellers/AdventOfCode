module Main where
import AdventLib
import Data.List

type RegionLengthRequirement = Int -> Bool

isValid :: RegionLengthRequirement -> String -> Bool
isValid regionLength s = all ($ s) [hasRunOfLength regionLength, neverDecrease 0 . digits]

hasRunOfLength :: Eq a => RegionLengthRequirement -> [a] -> Bool
hasRunOfLength req l = any req $ length <$> group l

neverDecrease :: Int -> [Int] -> Bool
neverDecrease _ [] = True
neverDecrease biggest (x:xs)
    | x >= biggest = neverDecrease x xs
    | otherwise    = False

main :: IO ()
main = do
    firstStar $ solve (>= 2)
    secondStar $ solve (== 2)
    where solve req = length $ filter (isValid req) (show <$> candidates)
          candidates = [152085..670283];
