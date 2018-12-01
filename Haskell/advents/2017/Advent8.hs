module Main where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import Parsing
import AdventLib

data Operation = Inc | Dec deriving Show

data Condition = Greater 
               | Lesser
               | GreaterEqual
               | LesserEqual
               | Equal
               | NotEqual
               deriving Show

applyCondition :: Condition -> Int -> Int -> Bool
applyCondition Greater = (>)
applyCondition Lesser = (<)
applyCondition GreaterEqual = (>=)
applyCondition LesserEqual = (<=)
applyCondition Equal = (==)
applyCondition NotEqual = (/=)

data Predicate = Predicate String Condition Int
    deriving Show

data Instruction = Instruction 
    { destination :: String
    , operation :: Operation
    , value :: Int
    , predicate :: Predicate
    } deriving Show

parseOperation :: Parser Operation
parseOperation = constSym "inc" Inc <|> constSym "dec" Dec

parseCondition :: Parser Condition
parseCondition = (try $ constSym ">=" GreaterEqual)
             <|> constSym ">" Greater
             <|> (try $ constSym "<=" LesserEqual)
             <|> constSym "<" Lesser
             <|> constSym "==" Equal
             <|> constSym "!=" NotEqual

parsePredicate :: Parser Predicate
parsePredicate = do
    var <- many1 letter
    spaces
    cond <- parseCondition
    spaces
    num <- parseInt
    return $ Predicate var cond num

parseInstruction :: Parser Instruction
parseInstruction = do
    dest <- many1 letter
    spaces
    operation <- parseOperation
    value <- parseInt
    spaces
    sym "if"
    pred <- parsePredicate
    return $ Instruction dest operation value pred 

type MachineState = Map.Map String Int

getVariable :: String -> MachineState -> Int
getVariable var state = Map.findWithDefault 0 var state

tryPredicate :: Predicate -> MachineState -> Bool
tryPredicate (Predicate var cond val) state = applyCondition cond (getVariable var state) val

runOperation :: Operation -> String -> Int -> MachineState -> MachineState
runOperation op var val state = Map.insert var newVal state
    where applyOp Inc a b = a + b
          applyOp Dec a b = a - b
          newVal = applyOp op (getVariable var state) val

runInstruction :: Instruction -> MachineState -> MachineState
runInstruction (Instruction dest op value pred) state
    | tryPredicate pred state = runOperation op dest value state
    | otherwise = state

main :: IO ()
main = do
    lines <- inputLines
    let Right instructions = sequence $ parse parseInstruction "" <$> lines
    let initial = Map.empty
    let states = scanl (flip runInstruction) initial instructions
    let maximums = (maximum . Map.elems) <$> states
    print $ last maximums
    print $ maximum (tail maximums)
