module Main where

import AdventLib
import Parsing
import Text.Parsec
import Control.Lens
import Data.List
import Data.List.Lens

data ScreenChange 
    = Rect Int Int 
    | RotRow Int Int
    | RotCol Int Int
    deriving Show

parseChange :: Parsec String () ScreenChange
parseChange = try rect <|> try rotrow <|> try rotcol
    where 
        rect = do
            string "rect" >> spaces
            x <- parseInt
            string "x"
            y <- parseInt
            return $ Rect x y
        rotrow = do
            string "rotate row y="
            y <- parseInt
            string " by "
            by <- parseInt
            return $ RotRow y by
        rotcol = do
            string "rotate column x="
            x <- parseInt
            string " by "
            by <- parseInt
            return $ RotCol x by

type Screen = [[Bool]]

initial = replicate 6 (replicate 50 False)

applyChange :: ScreenChange -> Screen -> Screen
applyChange (Rect x y) screen = foldr (\(x, y) s -> set s x y) screen [(x, y) | x <- [0..x-1], y <- [0..y-1]]
    where set :: Screen -> Int -> Int -> Screen 
          set scn x y = scn & ix y . ix x .~ True
applyChange (RotRow y by) screen = screen & ix y %~ rotate by
applyChange (RotCol x by) screen = transpose (transpose screen & ix x %~ rotate by)

main :: IO ()
main = do
    lines <- inputLines
    let Right moves = sequence $ parse parseChange "" <$> lines
    let screen = foldl (flip applyChange) initial moves
    putStrLn . bitshow $ screen
    print . length . filter id . concat $ screen 
