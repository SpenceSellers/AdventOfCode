module Main where
import AdventLib
import Data.List
import qualified Data.Tree as Tree
import Parsing
import Text.Parsec
import Data.Maybe

-- A program in raw form, holding just what you would know after parsing a single line.
data ProgramDef = ProgramDef
    { name :: String 
    , weight :: Int
    , childPrograms :: [String]
    } deriving Show

-- An entire tree of programs, including their names, weights, and children.
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
    where allChildren = progs >>= childPrograms -- All child programs. Anyone not in here is the root.

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
    single <- find (\l -> length l == 1) groups -- Which program is the only one with its weight?
    let incorrectProgram = fst . head $ single
    let childWeights = sum (programWeight <$> Tree.subForest incorrectProgram)
    -- The correct weight is the correct total weight of this tree minus the weights of the child programs.
    return (incorrectProgram, correctTotalWeight - childWeights)

    where weights = programWeight <$> progs
          sameWeight (_, w1) (_, w2) = w1 == w2
          groups = groupBy sameWeight (zip progs weights) -- The programs grouped by the total weights in their tree.
          -- Steal the correct weight from a sibling program. Just make sure we aren't choosing the wrong one.
          correctTotalWeight = snd . head . fromJust . find (\l -> length l /= 1) $ groups 

-- Recurses down the program tree and finds the program with an incorrect weight, if any.
findError :: ProgramTree -> Maybe (String, Int)
findError prog = do 
    (unique, correct) <- uniqueWeight (Tree.subForest prog)
    -- We now know that this tree contains the error.
    -- Recurse down to find it.
    return $ case findError unique of
        -- Found the error, return it
        Just s -> s
        -- Interesting, we didn't find the error in our children. 
        -- That means that WE are the error.
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
    -- Part 1: Find the root
    print $ rootLabel tree
    -- Part 2: Find the error
    print $ findError tree
