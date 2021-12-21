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

type DrawnNumbers = [String]

type Boards = [[String]]

type PuzzleInput = [String]

type BoardsInput = [String]

type BoardRow = [String]

type Board = [[(String, Marking)]]

data Marking = Marked | UnMarked deriving (Show, Eq)

solutionDay4Part1 :: PuzzleInput -> Int
solutionDay4Part1 = uncurry (*) . sumUnMarked . playBingo . prepareDrawnNumbers . prepareBoards . both removeEmptyRows . breakAtEmptyLine

-- TODO parse numbers early

sumUnMarked :: Maybe (Int, Board) -> (Int, Int)
sumUnMarked Nothing = (0, 0)
sumUnMarked (Just (lastDraw, winningBoard)) = (lastDraw, (sum . map derp . filter unMarked . concat) winningBoard)

derp :: (String, Marking) -> Int
derp (numberString, marking) = read numberString

unMarked :: (String, Marking) -> Bool
unMarked (_, marking) = marking == UnMarked

playBingo :: (DrawnNumbers, [Board]) -> Maybe (Int, Board)
playBingo numbersAndBoards
  | ([], _) <- numbersAndBoards = Nothing
  | (_, []) <- numbersAndBoards = Nothing
  | otherwise = Just (playBingo' numbers boards)
  where
    numbers = fst numbersAndBoards
    boards = snd numbersAndBoards

playBingo' :: [String] -> [Board] -> (Int, Board)
playBingo' numberStrings boards
  | any complete boardsAfterMarking = (number, head (filter complete boardsAfterMarking))
  | otherwise = playBingo' (tail numberStrings) boardsAfterMarking
  where
    number = (read . head) numberStrings
    boardsAfterMarking = map (map (map (mark (head numberStrings)))) boards

mark :: String -> (String, Marking) -> (String, Marking)
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

prepareBoards :: (DrawnNumbers, BoardsInput) -> (DrawnNumbers, [Board])
prepareBoards = fmap (chunksOf 5 . map (map (,UnMarked) . words))

prepareDrawnNumbers :: (DrawnNumbers, [Board]) -> (DrawnNumbers, [Board])
prepareDrawnNumbers (numbers, boards) = (concatMap (splitOn ",") numbers, boards)

breakAtEmptyLine :: PuzzleInput -> (DrawnNumbers, BoardsInput)
breakAtEmptyLine = break (== "")

removeEmptyRows :: PuzzleInput -> PuzzleInput
removeEmptyRows = filter (/= "")

solutionDay4Part2 :: [String] -> Int
solutionDay4Part2 = const 0