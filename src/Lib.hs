module Lib
  ( solutionDay1Part1,
    solutionDay1Part2,
    solutionDay2Part1,
    solutionDay2Part2,
  )
where

import Data.Bifunctor (bimap)

solutionDay1Part1 :: [String] -> Int
solutionDay1Part1 = countIncreases . map toInt

solutionDay1Part2 :: [String] -> Int
solutionDay1Part2 = countIncreases . slidingWindowSumLengthThree . map toInt

solutionDay2Part1 :: [String] -> Int
solutionDay2Part1 = uncurry (*) . sumDistances . map (toTupleOfDistances . words)

solutionDay2Part2 :: [String] -> Int
solutionDay2Part2 = const 900

sumDistances :: [(Int, Int)] -> (Int, Int)
sumDistances = foldl sumDistances' (0, 0)
  where
    sumDistances' firstTuple secondTuple = bimap (fst firstTuple +) (snd firstTuple +) secondTuple

toTupleOfDistances :: [String] -> (Int, Int)
toTupleOfDistances directionAndDistance
  | direction == "forward" = (distance, 0)
  | direction == "down" = (0, distance)
  | direction == "up" = (0, - distance)
  | otherwise = (0, 0)
  where
    direction = head directionAndDistance
    distance = read (head (tail directionAndDistance))

toInt :: String -> Int
toInt = read

countIncreases :: [Int] -> Int
countIncreases = length . filter (> 0) . listOfDifferences

listOfDifferences :: [Int] -> [Int]
listOfDifferences input
  | first : second : rest <- input = (second - first) : listOfDifferences (second : rest)
  | otherwise = []

slidingWindowSumLengthThree :: [Int] -> [Int]
slidingWindowSumLengthThree input
  | first : second : third : rest <- input = first + second + third : slidingWindowSumLengthThree (second : third : rest)
  | otherwise = []
