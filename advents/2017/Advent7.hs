module Main where
import AdventLib
import Data.List
import Control.Lens
import qualified Data.Tree as Tree
import Timing
import Parsing
import Text.Parsec
import Data.Maybe
import Safe

data ProgramDef = ProgramDef
    { name :: String 
    , weight :: Int
    , childPrograms :: [String]
    } deriving Show

type ProgramTree = Tree.Tree (String, Int)

parseProgram :: Parsec String () ProgramDef
parseProgram = do
    name <- many1 letter
    spaces
    weight <- bracketed "()" $ parseInt
    spaces
    children <- try parseChildren <|> return []

    return $ ProgramDef name weight children
    where parseChildren = sym "->" >> many1 letter `sepBy1` sym ","

-- Find the only program that isn't a child of another program
findRoot :: [ProgramDef] -> String
findRoot progs = head $ (name <$> progs) \\ allChildren
    where allChildren = progs >>= childPrograms 

buildTree :: [ProgramDef] -> ProgramTree
buildTree progs = Tree.unfoldTree treeDef rootName 
    where treeDef n = ((n, weight . byName $ n), childPrograms . byName $ n)
          byName n = getProgramByName progs n
          rootName = findRoot progs

getProgramByName :: [ProgramDef] -> String -> ProgramDef
getProgramByName progs progName = fromJust $ find (\x -> name x == progName) progs

programWeight :: ProgramTree -> Int
programWeight (Tree.Node (_, weight) children) = weight + sum (programWeight <$> children)

treeFilter :: (a -> Tree.Forest a -> Bool) -> Tree.Tree a -> Maybe (Tree.Tree a)
treeFilter f (Tree.Node label children) =
     fromBool (f label children) (Tree.Node label (catMaybes $ treeFilter f <$> children))

uniqueWeight :: [ProgramTree] -> Maybe (ProgramTree, Int)
uniqueWeight progs = do
    single <- find (\l -> length l == 1) groups
    let incorrectProgram = fst . head $ single
    let childWeights = sum (programWeight <$> Tree.subForest incorrectProgram)
    return (incorrectProgram, correctWeight - childWeights)

    where weights = programWeight <$> progs
          sameWeight (_, w1) (_, w2) = w1 == w2
          groups = groupBy sameWeight (zip progs weights)
          correctWeight = snd . head . fromJust . find (\l -> length l /= 1) $ groups

findError :: ProgramTree -> Maybe (String, Int)
findError prog = do 
    (unique, correct) <- uniqueWeight (Tree.subForest prog)
    return $ case findError unique of
        Just s -> s
        Nothing -> (fst . Tree.rootLabel $ unique, correct)

treeMap :: (Tree.Tree a -> b) -> Tree.Tree a -> Tree.Tree b
treeMap f t = Tree.Node (f t) (treeMap f <$> (Tree.subForest t))

readPrograms :: IO [ProgramDef]
readPrograms = do
    lines <- inputLines
    let Right programs = sequence $ parse parseProgram "" <$> lines
    return programs

main :: IO ()
main = do
    programs <- readPrograms
    let tree = buildTree programs
    print $ findError tree
