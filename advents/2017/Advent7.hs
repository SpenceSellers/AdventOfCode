module Main where
import AdventLib
import Data.List
import qualified Data.Tree as Tree
import Parsing
import Text.Parsec
import Data.Maybe

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

-- Constructs the tree of programs
buildTree :: [ProgramDef] -> ProgramTree
buildTree progs = Tree.unfoldTree treeDef rootName 
    where treeDef n = ((n, weight . byName $ n), childPrograms . byName $ n)
          byName n = getProgramByName progs n
          rootName = findRoot progs

getProgramByName :: [ProgramDef] -> String -> ProgramDef
getProgramByName progs progName = fromJust $ find (\x -> name x == progName) progs

-- Calculate the weight of a node and its children
programWeight :: ProgramTree -> Int
programWeight (Tree.Node (_, weight) children) = weight + sum (programWeight <$> children)

-- Given a list of programs, which ONE program has a different weight than the others, if any?
uniqueWeight :: [ProgramTree] -> Maybe (ProgramTree, Int)
uniqueWeight progs = do
    single <- find (\l -> length l == 1) groups
    let incorrectProgram = fst . head $ single
    let childWeights = sum (programWeight <$> Tree.subForest incorrectProgram)
    return (incorrectProgram, correctTotalWeight - childWeights)

    where weights = programWeight <$> progs
          sameWeight (_, w1) (_, w2) = w1 == w2
          groups = groupBy sameWeight (zip progs weights)
          correctTotalWeight = snd . head . fromJust . find (\l -> length l /= 1) $ groups

-- Recurses down the program tree and finds the program with an incorrect weight, if any.
findError :: ProgramTree -> Maybe (String, Int)
findError prog = do 
    (unique, correct) <- uniqueWeight (Tree.subForest prog)
    return $ case findError unique of
        Just s -> s
        Nothing -> (fst . Tree.rootLabel $ unique, correct)

readPrograms :: IO [ProgramDef]
readPrograms = do
    lines <- inputLines
    let Right programs = sequence $ parse parseProgram "" <$> lines
    return programs

main :: IO ()
main = do
    programs <- readPrograms
    let tree = buildTree programs
    print $ rootLabel tree
    print $ findError tree
