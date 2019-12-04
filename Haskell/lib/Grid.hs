module Grid where

import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right deriving (Eq, Show)
type Point = (Int, Int)

directionRelative :: Direction -> Point
directionRelative Up    = (0, -1)
directionRelative Down  = (0, 1)
directionRelative Left  = (-1, 0)
directionRelative Right = (1, 0)

readDirection :: String -> Maybe Direction
readDirection "D" = Just Down
readDirection "U" = Just Up
readDirection "L" = Just Left
readDirection "R" = Just Right
readDirection _ = Nothing

shiftPoint :: Point -> Direction -> Point
shiftPoint p d = shiftPointBy p d 1

shiftPointBy :: Point -> Direction -> Int -> Point
shiftPointBy p d n = p `addPoints` (scalePoint (directionRelative d) n)

scalePoint :: Point -> Int -> Point
scalePoint (x, y) scale = (x * scale, y * scale)

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

bound1d :: Int -> Int -> Int -> Int
bound1d mi ma x = max mi (min ma x)

-- Keep a point inside a bound, inclusive on both ends
bound :: Point -> Point -> Point -> Point
bound (mix, miy) (max, may) (x, y) = (bound1d mix max x, bound1d miy may y)

inBound :: Point -> Point -> Point -> Bool
inBound min max point = (bound min max point) == point

canMoveInBound :: Point -> Point -> Direction -> Bool
canMoveInBound max point dir = inBound (0,0) max $ shiftPoint point dir

manhattanDistanceFromOrigin :: Point -> Int
manhattanDistanceFromOrigin (x, y) = (abs x) + (abs y)