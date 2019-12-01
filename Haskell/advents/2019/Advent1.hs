{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import AdventLib

moduleMass :: Int -> Int
moduleMass mdule = (mdule `div` 3) - 2

massFuel :: Int -> Int
massFuel mass
    | additional <= 0 = mass
    | otherwise = mass + (massFuel additional)
    where additional = moduleMass mass

main :: IO ()
main = do
    ipts <- (fmap . fmap) read inputLines

    firstStar $ sum $ moduleMass <$> ipts
    secondStar $ sum $ (massFuel . moduleMass) <$> ipts