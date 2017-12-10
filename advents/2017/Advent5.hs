module Main where
import AdventLib
import Data.List
import qualified Data.Map as Map
import System.Clock
import Formatting
import Formatting.Clock

type ProgramMemory = Map.Map Int Int

data ProgramState = ProgramState Int ProgramMemory
    deriving Show

buildMemory :: [Int] -> ProgramMemory
buildMemory instructions = Map.fromList $ zip [0..] instructions 

-- Advance the program state by one tick
next :: Bool -> ProgramState -> Maybe ProgramState
next part2 (ProgramState pos mem) = do
    offset <- Map.lookup pos mem
    let increment = if part2 && offset >= 3 then (-1) else 1
    let incremented = Map.adjust (+ increment) pos mem
    return $ ProgramState (pos + offset) incremented

main :: IO ()
main = do
    start <- getTime Monotonic
    lines <- inputLines
    let instructions = read <$> lines
    let initialState = ProgramState 0 (buildMemory instructions)

    let part1next = next False
    let part2next = next True

    -- The answer is one less than our number of states (since the initial state doesn't count)
    let solve nxt = print $ (length $ iterateMaybe nxt initialState) - 1
    solve part1next
    solve part2next
    end <- getTime Monotonic
    fprint (timeSpecs) start end
