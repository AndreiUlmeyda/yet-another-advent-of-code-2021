module Day04
  ( solutionDay4Part1,
    solutionDay4Part2,
  )
where

import Data.List.Split
import Data.Tuple.Extra (both)

type DrawnNumbers = [String]

type Boards = [[String]]

type PuzzleInput = [String]

type BoardRow = String

-- solutionDay4Part1 = prepareDrawnNumbers . prepareRowsAndColumns . fmap removeEmptyRows . breakAtEmptyLine
solutionDay4Part1 = prepareDrawnNumbers . prepareRowsAndColumns . fmap removeEmptyRows . breakAtEmptyLine

prepareRowsAndColumns :: (DrawnNumbers, [String]) -> (DrawnNumbers, Boards)
prepareRowsAndColumns = fmap (map words) -- transform to 'prepareBoards'

prepareDrawnNumbers :: (DrawnNumbers, b) -> (DrawnNumbers, b)
prepareDrawnNumbers (numbers, boards) = (concatMap (splitOn ",") numbers, boards)

breakAtEmptyLine :: PuzzleInput -> (DrawnNumbers, [BoardRow])
breakAtEmptyLine = break (== "")

removeEmptyRows :: [BoardRow] -> [BoardRow]
removeEmptyRows = filter (/= "")

solutionDay4Part2 :: [String] -> Int
solutionDay4Part2 = const 0