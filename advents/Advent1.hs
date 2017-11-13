module Main where

import AdventLib
import Search



search = Search 1 [] (\s -> [Move (s + 1) 1, Move (s * 2) 2]) (\x -> x == 37)


main :: IO ()
main = do
  putStrLn $ show $ bfs search
