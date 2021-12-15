module Main where

import Lib
  ( solutionDay1Part1,
    solutionDay1Part2,
    solutionDay2Part1,
    solutionDay2Part2,
  )

main :: IO ()
main = interact (show . solutionDay2Part2 . lines)

-- TODO move solutions to separate modules, separate tests as well