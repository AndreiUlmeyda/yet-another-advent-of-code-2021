module Main where

import Day01
  ( solutionDay1Part1,
    solutionDay1Part2,
  )
import Day02
  ( solutionDay2Part1,
    solutionDay2Part2,
  )

main :: IO ()
main = interact (show . solutionDay2Part2 . lines)

-- TODO separate test files in line with modules