module Main where

import Lib
  ( solutionDay1Part1,
    solutionDay1Part2,
    solutionDay2Part1,
  )

main :: IO ()
main = interact (show . solutionDay2Part1 . lines)