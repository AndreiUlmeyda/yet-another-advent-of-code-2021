module Main where

import Day07

main :: IO ()
main = do
  actualData <- lines <$> readFile "puzzle-inputs/day-07"
  print $ solutionDay7Part2 actualData