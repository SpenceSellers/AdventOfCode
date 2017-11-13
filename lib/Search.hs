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

bfs :: Search s a -> [Move s a]
bfs search = bfs' [search]

bfs' :: [Search s a] -> [Move s a]
bfs' (search:rest) = if (search ^. win) (search ^. state) 
    then search ^. history
    else bfs' $ rest ++ expandbfs search
bfs' [] = error "Ran out of states" -- todo make safe

applyMove :: Search s a -> Move s a -> Search s a
applyMove search move@(Move new' action') = search 
    & state .~ new' 
    & history <>~ [move]

expandbfs :: Search s a -> [Search s a]
expandbfs search = applyMove search <$> moves
    where moves = search ^. childNodes $ search ^. state

--bfs' :: Search s a -> 

-- expandbfs :: SearchNode s a -> [Move s a] -> [SearchNode s a]
-- expandbfs node history = children node