module Day04
  ( solutionDay4Part1,
    solutionDay4Part2,
    playBingo,
    Marking (Marked, UnMarked),
    toWin,
  )
where

import Data.List (transpose)
import Data.List.Split
  ( chunksOf,
    splitOn,
  )
import Data.Tuple.Extra (both)

type DrawnNumbersInput = [String]

type DrawnNumbers = [Int]

type Boards = [[Int]]

type PuzzleInput = [String]

type BoardsInput = [String]

type Board = [[(Int, Marking)]]

data Marking = Marked | UnMarked deriving (Show, Eq)

solutionDay4Part1 :: PuzzleInput -> Int
solutionDay4Part1 = uncurry (*) . sumUnMarked . playBingo toWin . preparePuzzleInput

preparePuzzleInput :: PuzzleInput -> (DrawnNumbers, [Board])
preparePuzzleInput = prepareDrawnNumbers . prepareBoards . both removeEmptyRows . breakAtEmptyLine

sumUnMarked :: Maybe (Int, Board) -> (Int, Int)
sumUnMarked Nothing = (0, 0)
sumUnMarked (Just drawnNumberAndBoards) = fmap (sum . map fst . filter isUnMarked . concat) drawnNumberAndBoards

isUnMarked :: (Int, Marking) -> Bool
isUnMarked (_, marking) = marking == UnMarked

playBingo :: (DrawnNumbers -> [Board] -> (Int, Board)) -> (DrawnNumbers, [Board]) -> Maybe (Int, Board)
playBingo strategy numbersAndBoards
  | ([], _) <- numbersAndBoards = Nothing
  | (_, []) <- numbersAndBoards = Nothing
  | otherwise = Just (strategy numbers boards)
  where
    numbers = fst numbersAndBoards
    boards = snd numbersAndBoards

toWin :: DrawnNumbers -> [Board] -> (Int, Board)
toWin numberStrings boards
  | any complete boardsAfterMarking = (number, head (filter complete boardsAfterMarking))
  | otherwise = toWin (tail numberStrings) boardsAfterMarking
  where
    number = head numberStrings
    boardsAfterMarking = map (map (map (mark (head numberStrings)))) boards

mark :: Int -> (Int, Marking) -> (Int, Marking)
mark drawnNumber bingoCell
  | drawnNumber == fst bingoCell = (fst bingoCell, Marked)
  | otherwise = bingoCell

complete :: Board -> Bool
complete board = any rowComplete board || any rowComplete (transpose board)

rowComplete :: [(a, Marking)] -> Bool
rowComplete = all (snd . fmap isMarked)

isMarked :: Marking -> Bool
isMarked Marked = True
isMarked UnMarked = False

prepareBoards :: (DrawnNumbersInput, BoardsInput) -> (DrawnNumbersInput, [Board])
prepareBoards = fmap (chunksOf 5 . map (map ((,UnMarked) . read) . words))

prepareDrawnNumbers :: (DrawnNumbersInput, [Board]) -> (DrawnNumbers, [Board])
prepareDrawnNumbers (numbers, boards) = (concatMap (map read . splitOn ",") numbers, boards)

breakAtEmptyLine :: PuzzleInput -> (DrawnNumbersInput, BoardsInput)
breakAtEmptyLine = break (== "")

removeEmptyRows :: PuzzleInput -> PuzzleInput
removeEmptyRows = filter (/= "")

solutionDay4Part2 :: PuzzleInput -> Int
solutionDay4Part2 = uncurry (*) . sumUnMarked . playBingo toLose . preparePuzzleInput

toLose :: DrawnNumbers -> [Board] -> (Int, Board)
toLose numberStrings boards
  | True = (0, [])