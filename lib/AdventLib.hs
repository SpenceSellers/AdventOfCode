module AdventLib where

import qualified System.Environment
import Data.List.Split as Split
import Data.Maybe
import Data.List
import Safe

firstArg :: IO (Maybe String)
firstArg = headMay <$> System.Environment.getArgs

inputLines :: IO [String]
inputLines = do
    filename <- fromMaybe "input.txt" <$> firstArg 
    lines <$> readFile filename
