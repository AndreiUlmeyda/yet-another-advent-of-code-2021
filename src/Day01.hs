module Day01 (solutionDay1Part1, solutionDay1Part2) where

-- ######### Part One #########
solutionDay1Part1 :: [String] -> Int
solutionDay1Part1 = countIncreases . map read

countIncreases :: [Int] -> Int
countIncreases = length . filter (> 0) . listOfDifferences

listOfDifferences :: [Int] -> [Int]
listOfDifferences input
  | first : second : rest <- input = (second - first) : listOfDifferences (second : rest)
  | otherwise = []

-- ######### Part Two #########
solutionDay1Part2 :: [String] -> Int
solutionDay1Part2 = countIncreases . slidingWindowSumLengthThree . map read

slidingWindowSumLengthThree :: [Int] -> [Int]
slidingWindowSumLengthThree input
  | first : second : third : rest <- input = first + second + third : slidingWindowSumLengthThree (second : third : rest)
  | otherwise = []