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
    modules <- read <$$> inputLines
    firstStar . sum . fmap moduleMass $ modules
    secondStar . sum . fmap (massFuel . moduleMass) $ modules