module Day07
  ( solutionDay7Part1,
  -- solutionDay7Part2,
  )
where

import Data.List (sort)
import Data.List.Split (splitOn)
import Day04 (PuzzleInput)

type AlignmentCost = Int

type CrabPosition = Int

type TargetPosition = Int

solutionDay7Part1 :: PuzzleInput -> AlignmentCost
solutionDay7Part1 = cheapestAlignment . map read . splitOn "," . head

cheapestAlignment :: [CrabPosition] -> AlignmentCost
cheapestAlignment crabPositions = minimum $ map (alignPositions crabPositions) possiblePositions
  where
    possiblePositions = [(minimum crabPositions) .. (maximum crabPositions)]

alignPositions :: [CrabPosition] -> TargetPosition -> AlignmentCost
alignPositions crabPositions targetAlignment = sum $ map (align targetAlignment) crabPositions

align :: CrabPosition -> TargetPosition -> AlignmentCost
align target crabPosition = abs $ target - crabPosition

-- solutionDay7Part2 = const 0