module Main where

import Data.Bifunctor (bimap)

main :: IO ()
main = interact (show . uncurry (*) . sumDistances . map (toTupleOfDistances . words) . lines)

-- day 1 part 1 main = interact (show . countIncreases . map toInt . lines)
-- day 1 part 2 main = interact (show . countIncreases . slidingWindowSumLengthThree . map toInt . lines)
-- day 2 part 1 main = interact (show . uncurry (*) . sumDistances . map (toTupleOfDistances . words) . lines)

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