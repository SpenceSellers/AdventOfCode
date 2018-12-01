module Main where
import AdventLib
import Data.List
import Control.Lens
import qualified Data.Set as Set
import Timing

input :: [Int]
input = [10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6]

next :: [Int] -> [Int]
next state = distribute (index + 1) max (state & ix index .~ 0)
    where Just index = elemIndex max state
          max = maximum state

distribute :: Int -> Int -> [Int] -> [Int]
distribute _ 0 state     = state
distribute start n state = distribute (realPos + 1) (n - 1) (state & ix realPos %~ (+ 1))
    where realPos = start `mod` length state

-- Find the index of the first duplicate element in a list.
firstDuplicate :: Ord a => [a] -> Maybe Int
firstDuplicate elems = firstDuplicate' 0 Set.empty elems

firstDuplicate' :: Ord a => Int -> Set.Set a -> [a] -> Maybe Int
firstDuplicate' _ _ [] = Nothing
firstDuplicate' idx seen (x:xs)
    | Set.member x seen = Just idx
    | otherwise = firstDuplicate' (idx + 1) (Set.insert x seen) xs

main :: IO ()
main = do
    start <- startTiming
    -- An infinite list of successive states, continuing forever.
    let states = iterate next input

    -- What's the index of the first duplicate state?
    let Just firstIndex = firstDuplicate states

    -- What's the index of the state that matches the first duplicate AFTER that first duplicate?
    let Just secondIndex = elemIndex (states !! firstIndex) (drop (firstIndex + 1) states) 

    print firstIndex
    print (secondIndex + 1) -- We dropped the initial state itself, add one to not forget it.

    showTiming start