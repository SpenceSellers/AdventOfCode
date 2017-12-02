module Main where
import AdventLib    
import Data.List.Split


tabbed :: String -> [Int]
tabbed = fmap read . splitOn "\t"

minMaxDiff :: [Int] -> Int
minMaxDiff list = maximum list - minimum list

divDiff :: [Int] -> Int
divDiff list = a `div` b
    where combinations = [(x, y) | x <- list, y <- list, x /= y, x `rem` y == 0]
          (a, b) = head combinations

main :: IO ()
main = do
    input <- inputLines
    let nums = tabbed <$> input
    let partOne = sum $ minMaxDiff <$> nums
    let partTwo = sum $ divDiff <$> nums
    print partOne
    print partTwo
