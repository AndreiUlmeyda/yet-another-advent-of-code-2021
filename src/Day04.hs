module Day04
  ( solutionDay4Part1,
    solutionDay4Part2,
    playBingo,
    Marking (Marked, UnMarked),
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
solutionDay4Part1 = uncurry (*) . sumUnMarked . playBingo . preparePuzzleInput

preparePuzzleInput :: PuzzleInput -> (DrawnNumbers, [Board])
preparePuzzleInput = prepareDrawnNumbers . prepareBoards . both removeEmptyRows . breakAtEmptyLine

sumUnMarked :: Maybe (Int, Board) -> (Int, Int)
sumUnMarked Nothing = (0, 0)
sumUnMarked (Just (lastDraw, winningBoard)) = (lastDraw, (sum . map fst . filter unMarked . concat) winningBoard)

unMarked :: (Int, Marking) -> Bool
unMarked (_, marking) = marking == UnMarked

playBingo :: (DrawnNumbers, [Board]) -> Maybe (Int, Board)
playBingo numbersAndBoards
  | ([], _) <- numbersAndBoards = Nothing
  | (_, []) <- numbersAndBoards = Nothing
  | otherwise = Just (playBingo' numbers boards)
  where
    numbers = fst numbersAndBoards
    boards = snd numbersAndBoards

playBingo' :: DrawnNumbers -> [Board] -> (Int, Board)
playBingo' numberStrings boards
  | any complete boardsAfterMarking = (number, head (filter complete boardsAfterMarking))
  | otherwise = playBingo' (tail numberStrings) boardsAfterMarking
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
rowComplete = all isMarked

isMarked :: (a, Marking) -> Bool
isMarked (_, Marked) = True
isMarked (_, UnMarked) = False

boardScore :: (Int, Board) -> Int
boardScore = const 0

prepareBoards :: (DrawnNumbersInput, BoardsInput) -> (DrawnNumbersInput, [Board])
prepareBoards = fmap (chunksOf 5 . map (map ((,UnMarked) . strToInt) . words))

strToInt :: String -> Int
strToInt = read

prepareDrawnNumbers :: (DrawnNumbersInput, [Board]) -> (DrawnNumbers, [Board])
prepareDrawnNumbers (numbers, boards) = (concatMap (map read . splitOn ",") numbers, boards)

breakAtEmptyLine :: PuzzleInput -> (DrawnNumbersInput, BoardsInput)
breakAtEmptyLine = break (== "")

removeEmptyRows :: PuzzleInput -> PuzzleInput
removeEmptyRows = filter (/= "")

solutionDay4Part2 :: [String] -> Int
solutionDay4Part2 = const 0