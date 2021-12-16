module Main where

import Day01
  ( solutionDay1Part1,
    solutionDay1Part2,
  )
import Day02
  ( solutionDay2Part1,
    solutionDay2Part2,
  )
import Day03
  ( solutionDay3Part1,
    solutionDay3Part2,
  )

main :: IO ()
main = interact (show . solutionDay3Part1 . lines)

-- TODO separate test files in line with modules
-- TODO Substitute Text for String