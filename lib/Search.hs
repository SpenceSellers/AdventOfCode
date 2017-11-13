{-# LANGUAGE TemplateHaskell #-}
module Search where

import Control.Lens

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

type SearchCombiner s a = [Search s a] -> [Search s a] -> [Search s a]

bfs, dfs :: Search s a -> Maybe [Move s a]
bfs search = coreSearch (\new old -> old ++ new) [search]
dfs search = coreSearch (\new old -> new ++ old) [search]

coreSearch :: SearchCombiner s a -> [Search s a] -> Maybe [Move s a]
coreSearch combiner (search:rest) = if (search ^. win) (search ^. state) 
    then Just $ search ^. history
    else coreSearch combiner $ combiner (expand search) rest
coreSearch _ [] = Nothing

applyMove :: Search s a -> Move s a -> Search s a
applyMove search move@(Move new' action') = search 
    & state .~ new' 
    & history <>~ [move]

expand :: Search s a -> [Search s a]
expand search = applyMove search <$> moves
    where moves = search ^. childNodes $ search ^. state