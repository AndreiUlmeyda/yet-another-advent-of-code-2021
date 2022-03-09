module Day07
  ( solutionDay7Part1,
    solutionDay7Part2,
  )
where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List.Split (splitOn)
import Day04 (PuzzleInput)
import Prelude

type AlignmentCost = Int

type CrabPosition = Int

type TargetPosition = Int

type CostFunction = CrabPosition -> TargetPosition -> AlignmentCost

-- ######### Part One #########
solutionDay7Part1 :: PuzzleInput -> AlignmentCost
solutionDay7Part1 = cheapestAlignmentWith alignmentCost . prepareInput

prepareInput :: PuzzleInput -> [CrabPosition]
prepareInput = map read . splitOn "," . head

cheapestAlignmentWith :: CostFunction -> [CrabPosition] -> AlignmentCost
cheapestAlignmentWith costFunction crabPositions = minimum $ parMap rdeepseq (alignAtPosition costFunction crabPositions) possiblePositions
  where
    possiblePositions = [(minimum crabPositions) .. (maximum crabPositions)]

alignAtPosition :: CostFunction -> [CrabPosition] -> (TargetPosition -> AlignmentCost)
alignAtPosition costFunction crabPositions targetAlignment = sum $ map (costFunction targetAlignment) crabPositions

alignmentCost :: CrabPosition -> TargetPosition -> AlignmentCost
alignmentCost target crabPosition = abs (target - crabPosition)

-- ######### Part Two #########
solutionDay7Part2 :: PuzzleInput -> AlignmentCost
solutionDay7Part2 = cheapestAlignmentWith increasedAlignmentCost . prepareInput

-- | The alignment cost function for part two is a partial sum of the natural numbers.
-- These are related to, for instance, triangle numbers https://en.wikipedia.org/wiki/Triangular_number
-- and can be computed more efficiently than naively building up the list summing it up.
increasedAlignmentCost :: CrabPosition -> TargetPosition -> AlignmentCost
increasedAlignmentCost target crabPosition = (n * (n + 1)) `div` 2
  where
    n = alignmentCost target crabPosition
