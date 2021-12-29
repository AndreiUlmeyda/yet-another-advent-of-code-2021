module Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
where

import Data.Array.Unboxed (UArray, array, bounds, inRange, indices, listArray, (!))
import Data.Char (digitToInt)

-- solutionDay9Part1 :: [String] -> Int
solutionDay9Part1 input = (listOfNeighbors . toArray input . parseInput) input

toArray :: [String] -> [Int] -> UArray Int Int
toArray input = listArray (0, getArraySize input - 1)

-- probably needs actually indexing with tuples to avoid row indices wrapping around, maybe drop UArrays as well
listOfNeighbors :: UArray Int Int -> [[Int]]
listOfNeighbors smokeMeasurements = map listOfNeighborIndices (indices smokeMeasurements)
  where
    listOfNeighborIndices targetIndex = map (smokeMeasurements !) $filter validIndex [targetIndex + neighboringRows + neighboringColumns | neighboringRows <- [-1 .. 1], neighboringColumns <- map (* 10) [-1 .. 1]]
    (sizeX, sizeY) = bounds smokeMeasurements
    validIndex = inRange (sizeX, sizeY)

parseInput :: [String] -> [Int]
parseInput = map digitToInt . concat

getArraySize :: [String] -> Int
getArraySize a = length a * length (head a)

solutionDay9Part2 = const 0
