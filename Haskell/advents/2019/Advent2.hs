module Main where
import AdventLib
import Data.List.Split
import Control.Lens
import Debug.Trace

parseInput :: String -> [Int]
parseInput s = read <$> splitOn "," s

executeStep :: [Int] -> Int -> Maybe [Int]
executeStep state pos = case output of
    Just output -> Just $ state & ix outputIndex .~ output
    Nothing -> Nothing
    where [op, inputIndex1, inputIndex2, outputIndex] = traceShowId $ take 4 $ drop pos state
        --   Just op = state ^? ix pos
          Just input1 = state ^? ix inputIndex1
          Just input2 = state ^? ix inputIndex2
          output = applyOp op input1 input2

-- readThree :: [a] -> Maybe (a, b, c)
-- readThree [a, b, c] = Just (a, b, c)
-- readThree _ = Nothing

applyOp :: Int -> Int -> Int -> Maybe Int
applyOp 1 a b = Just $ a + b
applyOp 2 a b = Just $ a * b
applyOp 99 _ _ = Nothing

executeAll :: [Int] -> Int -> [[Int]]
executeAll state pos = case next of
    Nothing -> []
    Just next -> next : executeAll next (pos + 4)
    where next = executeStep state pos

main :: IO ()
main = do
    initial <- parseInput <$> inputFile
    print initial
    firstStar $ take 5 $ executeAll initial 0