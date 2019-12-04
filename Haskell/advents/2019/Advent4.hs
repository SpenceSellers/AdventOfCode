module Main where
import AdventLib
import Data.List

type RegionLengthRequirement = Int -> Bool

isValid :: RegionLengthRequirement -> String -> Bool
isValid regionLength s = (hasRunOfLength regionLength s) && (neverDecrease 0 $ digits s)

digits :: String -> [Int]
digits s = read <$> digitStrings
    where digitStrings = (\x -> [x]) <$> s

hasRunOfLength :: Eq a => RegionLengthRequirement -> [a] -> Bool
hasRunOfLength req l = any req $ length <$> group l

neverDecrease :: Int -> [Int] -> Bool
neverDecrease biggest (x:xs) = if x >= biggest then neverDecrease x xs else False
neverDecrease _ [] = True

main :: IO ()
main = do
    firstStar $ solve (>= 2)
    secondStar $ solve (== 2)
    where solve req = length $ filter (isValid req) (show <$> candidates)
          candidates = [152085..670283];
