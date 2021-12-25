module Day08
  ( solutionDay8Part1,
  -- solutionDay8Part2,
  )
where

import Data.List.Split (splitOn)
import Day04 (PuzzleInput)

type SevenSegmentDigit = String

-- ######### Part One #########
solutionDay8Part1 :: PuzzleInput -> Int
solutionDay8Part1 = countUniqueLengths . prepareInput

countUniqueLengths :: [SevenSegmentDigit] -> Int
countUniqueLengths = length . filter isOfUniqueLength

isOfUniqueLength :: Foldable t => t a -> Bool
isOfUniqueLength a = length a `elem` [2, 3, 4, 7]

prepareInput :: PuzzleInput -> [SevenSegmentDigit]
prepareInput = words . (!! 1) . splitOn " | " . head

-- ######### Part Two #########
-- solutionDay8Part2 :: PuzzleInput -> Int
-- solutionDay8Part2 = const 0
