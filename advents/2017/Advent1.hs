module Main where
import AdventLib    

ints :: String -> [Int]
ints s = read . pure <$> s

one :: [Int] -> Int
one nums = one' nums'
    where nums' = nums ++ [head nums]

one' :: [Int] -> Int 
one' (x:next:xs) = if x == next then x + (one' (next:xs)) else one' (next:xs)
one' _ = 0

two :: [Int] -> Int
two l = sum . map (comp . dual) $ [0..length l-1]
    where dual i = (l !! i, l !! halfIndex i)
          halfIndex i = (i + (length l) `div` 2) `mod` length l
          comp (a, b) = if a == b then a else 0

main :: IO ()
main = do
    input <- inputFile
    print . one . ints $ input
    print . two . ints $ input

    