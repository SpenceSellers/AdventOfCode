module Main where
import AdventLib    
import Data.List.Split
import Data.List

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates l = nub l /= l

main :: IO ()
main = do
    wordLines <- (fmap . fmap) (splitOn " ") inputLines
    let solve = print . length . filter (not . hasDuplicates)
    solve wordLines
    solve $ (fmap . fmap) sort wordLines