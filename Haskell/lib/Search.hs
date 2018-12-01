{-# LANGUAGE TemplateHaskell #-}
module Search where

import Control.Lens
import Data.List as List

data Search s = Search 
    { _state :: s
    , _history :: [s]
    , _childNodes :: s -> [s]
    , _win :: s -> Bool
    }

makeLenses ''Search

instance Show s => Show (Search s) where
    show s = "Search " ++ show (s ^. state)

isWinning :: Search s -> Bool
isWinning search = (search ^. win) (search ^. state) 


makeChildGen :: (s -> [a]) -> (s -> a -> s) -> s -> [s]
makeChildGen genActions transition state = transition state <$> genActions state

class FutureNodes n where
    next :: n a -> Maybe (a, n a)
    addNodes :: [a] -> n a -> n a

data SimpleSearchType = BFS | DFS;
data SimpleNodes a = SimpleNodes SimpleSearchType [a]

instance FutureNodes SimpleNodes where
    next (SimpleNodes dir nodes) = List.uncons nodes & _Just . _2 %~ SimpleNodes dir
    addNodes new (SimpleNodes DFS old) = SimpleNodes DFS $ new ++ old
    addNodes new (SimpleNodes BFS old) = SimpleNodes BFS $ old ++ new
    
bfs, dfs :: Search s -> Maybe [s]
bfs search = coreSearch $ SimpleNodes BFS [search]
dfs search = coreSearch $ SimpleNodes DFS [search]

coreSearch :: FutureNodes f => f (Search s) -> Maybe [s]
coreSearch nodes = case next nodes of
    Just (search, rest) -> if isWinning search
        then Just $ search ^. history
        else coreSearch $ addNodes (expand search) rest
    Nothing -> Nothing

applyMove :: Search s -> s -> Search s
applyMove search move = search 
    & state .~ move
    & history <>~ [move]

expand :: Search s -> [Search s]
expand search = applyMove search <$> moves
    where moves = search ^. childNodes $ search ^. state

-- Provides the final state of every possible path
-- Only use this if there are no loops, and every path ends in a winning state!
fullSearch :: Search s -> [s]
fullSearch search = 
    if isWinning search
    then [search ^. state]
    else concat (fullSearch <$> expand search)
