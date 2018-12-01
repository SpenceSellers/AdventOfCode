module Main where
import AdventLib    
import qualified Grid
import qualified Data.Map as Map
import Data.Maybe
import Data.List

-- After we turn a corner, which direction will we end up going?
nextDirection :: Grid.Direction -> Grid.Direction
nextDirection Grid.Right = Grid.Up
nextDirection Grid.Up = Grid.Left
nextDirection Grid.Left = Grid.Down
nextDirection Grid.Down = Grid.Right

-- The intermediate state while we build the spiral.
data GridState = GridState 
    Grid.Direction -- Which direction we'll go next
    Int            -- The number we're on
    Int            -- The length of the row we're on
    Int            -- Distance left in the row we're on
    (Int, Int)     -- Our position
    deriving Show

-- Is this corner going to extend our reach? Not all do.
isLongerCorner :: Grid.Direction -> Bool
isLongerCorner Grid.Left = True
isLongerCorner Grid.Right = True
isLongerCorner _ = False

nextState :: GridState -> GridState
nextState (GridState d n rowlen 0 pos) = GridState (nextDirection d) (n + 1) nextRowLength (rowlen + 1) (Grid.shiftPoint pos d)
    where nextRowLength = if isLongerCorner d
            then rowlen + 1
            else rowlen
nextState (GridState d n rowlen remaining pos) = GridState d (n + 1) rowlen (remaining - 1) (Grid.shiftPoint pos d)

initial :: GridState
initial = GridState Grid.Right 1 (-1) 0 (0, 0)

-- List of all coordinates in the spiral, in order
coords :: [(Int, Int)]
coords = position <$> iterate nextState initial
    where position (GridState _ _ _ _ pos) = pos

coordOf :: Int -> (Int, Int)
coordOf n = coords !! (n - 1)

-- A snapshot in time during the grid summations
type NumberState = Map.Map (Int, Int) Int

-- Sum up the numbers adjacent to a position
adjacentSums :: NumberState -> (Int, Int) -> Int
adjacentSums ns pos = sum $ valueOrZero <$> positions
    where offsets = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]
          positions = Grid.addPoints pos <$> offsets
          valueOrZero p = fromMaybe 0 $ Map.lookup p ns

calcNext :: NumberState -> (Int, Int) -> NumberState
calcNext ns pos = Map.insert pos (adjacentSums ns pos) ns

main = do
    let input = 347991 
    let (x, y) = coordOf input
    print $ (abs x) + (abs y)

    let initialState = Map.insert (0, 0) 1 Map.empty 

    -- Get the first 100 grid sums. The numbers grow so fast that 100 is overkill.
    let sums = foldl calcNext initialState (take 100 . drop 1 $ coords)
    let values = Map.elems sums

    -- Print the first value bigger than our input
    print $ head . filter (> input) $ sort values
