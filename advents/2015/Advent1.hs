module Main where

import AdventLib
import Data.List

main, a1, a2 :: IO ()
main = a1 >> a2

a1 = print . sum . map upDown =<< inputFile 

a2 = print . elemIndex (-1) . scanl (+) 0 . map upDown =<< inputFile

upDown :: Char -> Int 
upDown '(' = 1
upDown ')' = -1