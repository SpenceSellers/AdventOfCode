module Main where
import AdventLib
import Grid
import Text.Parsec
import Text.Parsec.String
import Parsing
import Data.Maybe
import Data.List
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set

buildMap :: Int -> Point -> [(Direction, Int)] -> Map.Map Point Int -> Map.Map Point Int
buildMap step pos [] existing = existing
buildMap step pos (x: xs) existing = buildMap (step + n) (shiftPointBy pos direction n) xs nextMap
    where (direction, n) = x
          pointsCrossed = (\shift -> (shift, shiftPointBy pos direction shift)) <$> [1..n] 
          nextMap = foldr (\(shift, point) m -> Map.insert point (step + shift) m) existing pointsCrossed 

main :: IO ()
main = do
    inputStrings <- inputLines
    let [Prelude.Right input1, Prelude.Right input2] = parse (commaSeparated letterNumber) "" <$> inputStrings
    let instructions = map (\(d, n) -> (fromJust $ readDirection [d], n)) <$> [input1, input2]
    let completeMaps = (\insts -> buildMap 0 (0, 0) insts Map.empty) <$> instructions
    let [map1, map2] = completeMaps

    firstStar $ sortOn snd $ Map.toList $ Map.intersectionWith (+) map1 map2
