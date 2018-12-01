module Main where
    
import AdventLib
import Control.Lens
import Data.List
import Data.List.Lens
import Data.Maybe
import qualified Grid as Grid
import qualified Search as Search

isOpen :: Char -> Bool
isOpen c = elem c "bcdef"

hashDirections :: String -> [Grid.Direction]
hashDirections hash = catMaybes [ifb u Grid.Up, ifb d Grid.Down, ifb l Grid.Left, ifb r Grid.Right]
    where (u:d:l:r:_) = isOpen <$> hash
          ifb b dir = if b then Just dir else Nothing

directionChar :: Grid.Direction -> Char
directionChar Grid.Up    = 'U'
directionChar Grid.Down  = 'D'
directionChar Grid.Left  = 'L'
directionChar Grid.Right = 'R'

data PathState = PathState Grid.Point String deriving Show

childGen :: PathState -> [PathState]
childGen (PathState point str) = newState <$> directions
    where hash = md5 str
          directions = filter (Grid.canMoveInBound (3,3) point) . hashDirections $ hash
          newState dir = PathState (Grid.shiftPoint point dir) (str ++ [directionChar dir])

hasWon :: PathState -> Bool
hasWon (PathState point _) = point == (3,3)

initialState :: PathState
initialState = PathState (0,0) "njfxhljp"

search = Search.Search initialState [] childGen hasWon

main :: IO ()
main = do
    print (last <$> Search.bfs search)
    print . maximum . map (\(PathState p s) -> length . filter (flip elem "UDLR") $ s) . Search.fullSearch $ search