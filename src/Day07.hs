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
solutionDay7Part1 = cheapestAlignment alignmentCost . prepareInput

prepareInput :: PuzzleInput -> [CrabPosition]
prepareInput = map read . splitOn "," . head

cheapestAlignment :: CostFunction -> [CrabPosition] -> AlignmentCost
cheapestAlignment costFunction crabPositions = minimum $ map (alignPositions costFunction crabPositions) possiblePositions
  where
    possiblePositions = [(minimum crabPositions) .. (maximum crabPositions)]

alignPositions :: CostFunction -> [CrabPosition] -> TargetPosition -> AlignmentCost
alignPositions costFunction crabPositions targetAlignment = sum $ map (costFunction targetAlignment) crabPositions

align :: CrabPosition -> TargetPosition -> AlignmentCost
align target crabPosition = abs $ target - crabPosition

alignmentCost :: CrabPosition -> TargetPosition -> AlignmentCost
alignmentCost target crabPosition = abs $ target - crabPosition

-- ######### Part Two #########
solutionDay7Part2 :: PuzzleInput -> AlignmentCost
solutionDay7Part2 = cheapestAlignment alignmentCostPartTwo . prepareInput

alignmentCostPartTwo :: CrabPosition -> TargetPosition -> AlignmentCost
alignmentCostPartTwo target crabPosition = sum $ take (alignmentCost target crabPosition) [1 ..]
