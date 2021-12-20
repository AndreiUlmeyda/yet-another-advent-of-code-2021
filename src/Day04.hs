module Day04
  ( solutionDay4Part1,
    solutionDay4Part2,
  )
where

import Data.Tuple.Extra (both)

solutionDay4Part1 = prepareRowsAndColumns . both removeEmptyLines . breakAtEmptyLine

prepareRowsAndColumns :: ([String], [String]) -> ([String], [[String]])
prepareRowsAndColumns = fmap (map words)

breakAtEmptyLine :: [[Char]] -> ([[Char]], [[Char]])
breakAtEmptyLine = break (== "")

removeEmptyLines :: [[Char]] -> [[Char]]
removeEmptyLines = filter (/= "")

solutionDay4Part2 :: [String] -> Int
solutionDay4Part2 = const 0