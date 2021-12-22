module Day05
  ( solutionDay5Part1,
    solutionDay5Part2,
  )
where

import Data.List.Split (splitOn)
import Day04 (PuzzleInput)

data Line = MkLine
  { startX :: Int,
    startY :: Int,
    endX :: Int,
    endY :: Int
  }
  deriving (Show, Eq)

type Coordinate = (Int, Int)

--solutionDay5Part1 :: PuzzleInput -> [(String, String)]
solutionDay5Part1 = concatMap listOfCoordinatesFrom . prepareLines

listOfCoordinatesFrom :: Line -> [Coordinate]
listOfCoordinatesFrom (MkLine sx sy ex ey)
  | sx == ex = zip (fromSmallerToBigger sy ey) (repeat sx)
  | sy == ey = zip (fromSmallerToBigger sx ex) (repeat sy)
  | otherwise = error "tried to generate list of coordinates from diagonal line"
  where
    fromSmallerToBigger a b = [(min a b) .. (max a b)]

prepareLines :: PuzzleInput -> [Line]
prepareLines = filter notDiagonal . map (toLine . map read . concatMap (splitOn ",") . splitOn " -> ")

notDiagonal :: Line -> Bool
notDiagonal (MkLine sx sy ex ey) = sx == ex || sy == ey

toLine :: [Int] -> Line
toLine coordinates
  | [sx, sy, ex, ey] <- coordinates = MkLine sx sy ex ey
  | otherwise = error "couldn't parse line coordinates with more or less than 4 elements"

solutionDay5Part2 :: PuzzleInput -> Int
solutionDay5Part2 = const 0

-- (0,0) -> non-diagonal if x = const or y = const

-- 0,9 -> 5,9
-- 8,0 -> 0,8 drop
-- 9,4 -> 3,4
-- 2,2 -> 2,1
-- 7,0 -> 7,4
-- 6,4 -> 2,0
-- 0,9 -> 2,9
-- 3,4 -> 1,4
-- 0,0 -> 8,8
-- 5,5 -> 8,2