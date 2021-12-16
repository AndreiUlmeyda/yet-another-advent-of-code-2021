module Day01 (solutionDay1Part1, solutionDay1Part2) where

solutionDay1Part1 :: [String] -> Int
solutionDay1Part1 = countIncreases . map toInt

toInt :: String -> Int
toInt = read

countIncreases :: [Int] -> Int
countIncreases = length . filter (> 0) . listOfDifferences

listOfDifferences :: [Int] -> [Int]
listOfDifferences input
  | first : second : rest <- input = (second - first) : listOfDifferences (second : rest)
  | otherwise = []

solutionDay1Part2 :: [String] -> Int
solutionDay1Part2 = countIncreases . slidingWindowSumLengthThree . map toInt

slidingWindowSumLengthThree :: [Int] -> [Int]
slidingWindowSumLengthThree input
  | first : second : third : rest <- input = first + second + third : slidingWindowSumLengthThree (second : third : rest)
  | otherwise = []