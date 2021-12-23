module Day06
  ( solutionDay6Part1,
    solutionDay6Part2,
  )
where

import Data.List.Split (splitOn)
import Day04 (PuzzleInput)

solutionDay6Part1 :: PuzzleInput -> Int
solutionDay6Part1 = length . (!! 80) . iterate (concatMap ageAndMultiply) . map read . splitOn "," . head

ageAndMultiply :: Int -> [Int]
ageAndMultiply age
  | age == 0 = [6, 8]
  | otherwise = [age -1]

solutionDay6Part2 :: PuzzleInput -> Int
solutionDay6Part2 = const 0