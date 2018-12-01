module Main where
import AdventLib    

ints :: String -> [Int]
ints s = read . pure <$> s

sumOffset :: Int -> [Int] -> Int
sumOffset rotation list = sum $ zipWith compare list $ rotate rotation list
    where compare a b = if a == b then a else 0

main :: IO ()
main = do
    input <- inputFile
    let halfLen = length input `div` 2
    let solve offset = print . sumOffset offset . ints $ input
    solve 1       -- Part 1
    solve halfLen -- Part 2