module Day04
  ( solutionDay4Part1,
    solutionDay4Part2,
  )
where

import Data.List.Split
  ( chunksOf,
    splitOn,
  )
import Data.Tuple.Extra (both)

type DrawnNumbers = [String]

type Boards = [[String]]

type PuzzleInput = [String]

type BoardsInput = [String]

type BoardRow = [String]

type Board = [BoardRow]

data Marking = Marked | UnMarked deriving (Show, Eq)

solutionDay4Part1 = prepareDrawnNumbers . prepareBoards . both removeEmptyRows . breakAtEmptyLine

playBingo :: (DrawnNumbers, [[[(String, Marking)]]]) -> (Int, Board)
playBingo = const (0, [])

boardScore :: (Int, Board) -> Int
boardScore = const 0

prepareBoards :: (DrawnNumbers, BoardsInput) -> (DrawnNumbers, [[[(String, Marking)]]])
prepareBoards = fmap (chunksOf 5 . map (map (,UnMarked) . words))

prepareDrawnNumbers :: (DrawnNumbers, [[[(String, Marking)]]]) -> (DrawnNumbers, [[[(String, Marking)]]])
prepareDrawnNumbers (numbers, boards) = (concatMap (splitOn ",") numbers, boards)

breakAtEmptyLine :: PuzzleInput -> (DrawnNumbers, BoardsInput)
breakAtEmptyLine = break (== "")

removeEmptyRows :: PuzzleInput -> PuzzleInput
removeEmptyRows = filter (/= "")

solutionDay4Part2 :: [String] -> Int
solutionDay4Part2 = const 0