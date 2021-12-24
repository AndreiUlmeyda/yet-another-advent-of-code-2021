module Day07
  ( solutionDay7Part1,
    solutionDay7Part2,
  )
where

import Data.List (sort)
import Data.List.Split (splitOn)
import Day04 (PuzzleInput)

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
cheapestAlignmentWith costFunction crabPositions = minimum $ map (alignAtPosition costFunction crabPositions) possiblePositions
  where
    possiblePositions = [(minimum crabPositions) .. (maximum crabPositions)]

alignAtPosition :: CostFunction -> [CrabPosition] -> TargetPosition -> AlignmentCost
alignAtPosition costFunction crabPositions targetAlignment = sum $ map (costFunction targetAlignment) crabPositions

alignmentCost :: CrabPosition -> TargetPosition -> AlignmentCost
alignmentCost target crabPosition = abs (target - crabPosition)

-- ######### Part Two #########
solutionDay7Part2 :: PuzzleInput -> AlignmentCost
solutionDay7Part2 = cheapestAlignmentWith increasedAlignmentCost . prepareInput

increasedAlignmentCost :: CrabPosition -> TargetPosition -> AlignmentCost
increasedAlignmentCost target crabPosition = sum $ take (alignmentCost target crabPosition) [1 ..]
