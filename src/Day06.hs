module Day06
  ( solutionDay6Part1,
    solutionDay6Part2,
  )
where

import Control.Lens (element, set)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Day04 (PuzzleInput)

data FishPopulation = MkFishPopulation
  { age0 :: Int,
    age1 :: Int,
    age2 :: Int,
    age3 :: Int,
    age4 :: Int,
    age5 :: Int,
    age6 :: Int,
    age7 :: Int,
    age8 :: Int
  }
  deriving (Show, Eq)

-- ######### Part One #########
solutionDay6Part1 :: PuzzleInput -> Int
solutionDay6Part1 = populationSizeAfterDays 80

populationSizeAfterDays :: Int -> [[Char]] -> Int
populationSizeAfterDays days = length . (!! days) . iterate (concatMap ageAndMultiply) . prepareInput

prepareInput :: PuzzleInput -> [Int]
prepareInput = map read . splitOn "," . head

ageAndMultiply :: Int -> [Int]
ageAndMultiply age
  | age == 0 = [6, 8]
  | otherwise = [age -1]

-- ######### Part Two #########
solutionDay6Part2 :: PuzzleInput -> Int
solutionDay6Part2 = sum . map snd . (!! 256) . iterate ageAndMultiplyImproved . supplyMissingIndices zeroCounts . map toElementAndLength . group . sort . prepareInput

supplyMissingIndices :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
supplyMissingIndices defaults [] = defaults
supplyMissingIndices defaults [tuple] = set (element (fst tuple)) tuple defaults
supplyMissingIndices defaults (tuple : rest) = supplyMissingIndices (set (element (fst tuple)) tuple defaults) rest

zeroCounts :: [(Int, Int)]
zeroCounts = zip [0 .. 8] (repeat 0)

toElementAndLength :: [a] -> (a, Int)
toElementAndLength a = (head a, length a)

ageAndMultiplyImproved :: [(Int, Int)] -> [(Int, Int)]
ageAndMultiplyImproved agesAndCount =
  [ (0, snd (agesAndCount !! 1)),
    (1, snd (agesAndCount !! 2)),
    (2, snd (agesAndCount !! 3)),
    (3, snd (agesAndCount !! 4)),
    (4, snd (agesAndCount !! 5)),
    (5, snd (agesAndCount !! 6)),
    (6, snd (agesAndCount !! 7) + snd (head agesAndCount)),
    (7, snd (agesAndCount !! 8)),
    (8, snd (head agesAndCount))
  ]