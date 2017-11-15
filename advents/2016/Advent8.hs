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

parseChange :: Parsec String () ScreenChange
parseChange = try rect <|> try rotrow <|> try rotcol where 
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

initial :: Screen 
initial = replicate 6 (replicate 50 False)

squareCoords :: Int -> Int -> [(Int, Int)]
squareCoords x y= [(x, y) | x <- [0..x-1], y <- [0..y-1]]

applyChange :: ScreenChange -> Screen -> Screen
applyChange (Rect x y) = \s -> foldr set s (squareCoords x y)
    where set :: (Int, Int) -> Screen -> Screen 
          set (x, y) scn = scn & ix y . ix x .~ True
applyChange (RotRow y by) = ix y %~ rotate by
applyChange (RotCol x by) = transposed . ix x %~ rotate by

main :: IO ()
main = do
    lines <- inputLines
    let Right moves = sequence $ parse parseChange "" <$> lines
    let screen = foldl (flip applyChange) initial moves
    putStrLn . bitshow $ screen
    print . length . filter id . concat $ screen 
