module Main where
import AdventLib

moduleMass :: Int -> Int
moduleMass mdule = (mdule `div` 3) - 2

massFuel :: Int -> Int
massFuel mass
    | mass <= 0 = 0
    | otherwise = mass + (massFuel . moduleMass) mass

main :: IO ()
main = do
    modules <- (fmap . fmap) read inputLines

    firstStar $ sum $ moduleMass <$> modules
    secondStar $ sum $ (massFuel . moduleMass) <$> modules