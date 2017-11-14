{-# LANGUAGE TemplateHaskell #-}
module Search where

import Control.Lens
import Data.List as List

data Move s a = Move s a deriving Show

data Search s a = Search 
    { _state :: s
    , _history :: [Move s a]
    , _childNodes :: s -> [Move s a]
    , _win :: s -> Bool
    }

makeLenses ''Search

instance (Show s, Show a) => Show (Search s a) where
    show s = "Search " ++ show  (s ^. state)

class FutureNodes n where
    next :: n a -> Maybe (a, n a)
    addNodes :: [a] -> n a -> n a

data SimpleSearchType = BFS | DFS;
data SimpleNodes a = SimpleNodes SimpleSearchType [a]

instance FutureNodes SimpleNodes where
    next (SimpleNodes dir nodes) = List.uncons nodes & _Just . _2 %~ SimpleNodes dir
    addNodes new (SimpleNodes DFS old) = SimpleNodes DFS $ new ++ old
    addNodes new (SimpleNodes BFS old) = SimpleNodes BFS $ old ++ new
    
bfs, dfs :: Search s a -> Maybe [Move s a]
bfs search = coreSearch $ SimpleNodes BFS [search]
dfs search = coreSearch $ SimpleNodes DFS [search]

coreSearch :: FutureNodes f => f (Search s a) -> Maybe [Move s a]
coreSearch nodes = case next nodes of
    Just (search, rest) -> if (search ^. win) (search ^. state) 
        then Just $ search ^. history
        else coreSearch $ addNodes (expand search) rest
    Nothing -> Nothing

applyMove :: Search s a -> Move s a -> Search s a
applyMove search move@(Move new' action') = search 
    & state .~ new' 
    & history <>~ [move]

expand :: Search s a -> [Search s a]
expand search = applyMove search <$> moves
    where moves = search ^. childNodes $ search ^. state