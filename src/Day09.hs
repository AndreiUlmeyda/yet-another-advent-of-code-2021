module Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
where

import Data.Array.Unboxed (UArray, array, bounds, inRange, indices, (!))
import Data.Char (digitToInt)

solutionDay9Part1 :: [String] -> Int
solutionDay9Part1 = sum . computeDangerLevel . filter isMinimum . pairWithListOfNeighbors . toArray . parseInput

computeDangerLevel :: [(Int, b)] -> [Int]
computeDangerLevel = map ((+) 1 . fst)

isMinimum :: (Int, [Int]) -> Bool
isMinimum (element, list) = element == minimum list

toArray :: [Int] -> UArray (Int, Int) Int
toArray inputInts = array bds (zip [(a, b) | a <- [0 .. 4], b <- [0 .. 9]] inputInts)
  where
    bds = ((0, 0), (4, 9))

pairWithListOfNeighbors :: UArray (Int, Int) Int -> [(Int, [Int])]
pairWithListOfNeighbors smokeMeasurements = map listOfNeighborIndices (indices smokeMeasurements)
  where
    listOfNeighborIndices (targetX, targetY) = (smokeMeasurements ! (targetX, targetY), map (smokeMeasurements !) $ filter validIndex [(targetX + neighboringRows, targetY + neighboringColumns) | neighboringRows <- [-1 .. 1], neighboringColumns <- [-1 .. 1]])
    (sizeX, sizeY) = bounds smokeMeasurements
    validIndex = inRange $ bounds smokeMeasurements

parseInput :: [String] -> [Int]
parseInput = map digitToInt . concat

solutionDay9Part2 = const 0
