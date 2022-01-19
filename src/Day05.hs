module Day05
  ( solutionDay5Part1,
    solutionDay5Part2,
    listOfCoordinatesFrom,
    Line (MkLine),
    range,
  )
where

import Data.List (group, sortBy)
import Data.List.Split (splitOn)
import Day04 (PuzzleInput)
import Prelude

data Line = MkLine
  { startX :: Int,
    startY :: Int,
    endX :: Int,
    endY :: Int
  }
  deriving (Show, Eq)

type Coordinate = (Int, Int)

-- ######### Part One #########
solutionDay5Part1 :: PuzzleInput -> Int
solutionDay5Part1 = countCoordinatesWithLessThanTwoLines . filter notDiagonal . prepareLines

-- ######### Part Two #########
solutionDay5Part2 :: PuzzleInput -> Int
solutionDay5Part2 = countCoordinatesWithLessThanTwoLines . prepareLines

countCoordinatesWithLessThanTwoLines :: [Line] -> Int
countCoordinatesWithLessThanTwoLines = length . filter ((>= 2) . length) . group . sortCoordinates . concatMap listOfCoordinatesFrom

sortCoordinates :: [Coordinate] -> [Coordinate]
sortCoordinates = sortBy compareCoordinates

compareCoordinates :: Coordinate -> Coordinate -> Ordering
compareCoordinates coord1 coord2
  | fst coord1 == fst coord2 = compare (snd coord1) (snd coord2)
  | otherwise = compare (fst coord1) (fst coord2)

listOfCoordinatesFrom :: Line -> [Coordinate]
listOfCoordinatesFrom (MkLine sx sy ex ey)
  | sx == ex = zip (repeat sx) (range sy ey)
  | sy == ey = zip (range sx ex) (repeat sy)
  | otherwise = zip (range sx ex) (range sy ey)

-- adapted from https://stackoverflow.com/questions/7958181/ranges-in-haskell-ghci
range :: (Ord a, Num a) => a -> a -> [a]
range start end
  | start <= end = takeWhile (<= end) (iterate (+ 1) start)
  | otherwise = reverse (range end start)

prepareLines :: PuzzleInput -> [Line]
prepareLines = map (toLine . map read . concatMap (splitOn ",") . splitOn " -> ")

notDiagonal :: Line -> Bool
notDiagonal (MkLine sx sy ex ey) = sx == ex || sy == ey

toLine :: [Int] -> Line
toLine coordinates
  | [sx, sy, ex, ey] <- coordinates = MkLine sx sy ex ey
  | otherwise = error "couldn't parse line coordinates with more or less than 4 elements"

-- (0,0) -> non-diagonal if x = const or y = const

-- 0,9 -> 5,9
-- 8,0 -> 0,8 drop
-- 9,4 -> 3,4
-- 2,2 -> 2,1 ??
-- 7,0 -> 7,4
-- 6,4 -> 2,0 drop
-- 0,9 -> 2,9
-- 3,4 -> 1,4
-- 0,0 -> 8,8 drop
-- 5,5 -> 8,2 drop