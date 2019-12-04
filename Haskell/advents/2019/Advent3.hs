module Main where
import AdventLib
import Grid
import Text.Parsec
import Text.Parsec.String
import Parsing
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

buildMap :: Point -> [(Direction, Int)] -> Map.Map Point Int -> Map.Map Point Int
buildMap pos [] existing = existing
buildMap pos (x: xs) existing = buildMap (shiftPointBy pos direction n) xs nextMap
    where (direction, n) = x
          pointsCrossed = (\shift -> shiftPointBy pos direction shift) <$> [1..n] 
          nextMap = foldr (\point m -> Map.alter (\c -> Just $ fromMaybe 1 ((+1) <$> c)) point m) existing pointsCrossed
          
main :: IO ()
main = do
    inputStrings <- inputLines
    let [Prelude.Right input1, Prelude.Right input2] = parse (commaSeparated letterNumber) "" <$> inputStrings
    let instructions = map (\(d, n) -> (fromJust $ readDirection [d], n)) <$> [input1, input2]
    let completeMaps = (\insts -> buildMap (0, 0) insts Map.empty) <$> instructions
    let [map1, map2] = fmap (Set.fromList . fmap fst . Map.toList) completeMaps

    firstStar $ sortOn manhattanDistanceFromOrigin $ Set.toList $ Set.intersection map1 map2
