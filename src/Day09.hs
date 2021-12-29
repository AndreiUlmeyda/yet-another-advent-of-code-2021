module Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
where

import Data.Array.Unboxed (UArray, array, bounds, inRange, indices, (!))
import Data.Char (digitToInt)
import Day04 (PuzzleInput)

type ElevationMeasurement = Int

type NeighboringMeasurement = ElevationMeasurement

type CoordinatePoint = (Int, Int)

type NeighboringCoordinatePoint = CoordinatePoint

type DangerLevel = Int

solutionDay9Part1 :: [String] -> Int
solutionDay9Part1 = sum . computeDangerLevel . filter isMinimum . pairWithListOfNeighbors . toArray . toInt

computeDangerLevel :: [(ElevationMeasurement, a)] -> [DangerLevel]
computeDangerLevel = map ((+) 1 . fst)

isMinimum :: (ElevationMeasurement, [NeighboringMeasurement]) -> Bool
isMinimum (measurement, neighboringMeasurements) = measurement == minimum neighboringMeasurements

toArray :: [[ElevationMeasurement]] -> UArray CoordinatePoint ElevationMeasurement
toArray input = array bds (zip [(a, b) | a <- [0 .. rowCount -1], b <- [0 .. columnCount -1]] (concat input))
  where
    (rowCount, columnCount) = (length input, length (head input))
    bds = ((0, 0), (rowCount - 1, columnCount - 1))

pairWithListOfNeighbors :: UArray CoordinatePoint ElevationMeasurement -> [(ElevationMeasurement, [NeighboringMeasurement])]
pairWithListOfNeighbors measurements = map listOfNeighbors (indices measurements)
  where
    listOfNeighbors (xCoord, yCoord) = (measurements ! (xCoord, yCoord), map (measurements !) $ filter validIndex (neighboringCoordinates (xCoord, yCoord)))
    validIndex = inRange $ bounds measurements

neighboringCoordinates :: CoordinatePoint -> [NeighboringCoordinatePoint]
neighboringCoordinates (xCoord, yCoord) =
  [ (xCoord + neighboringRows, yCoord + neighboringColumns)
    | neighboringRows <- [- immediateNeighborDistance .. immediateNeighborDistance],
      neighboringColumns <- [- immediateNeighborDistance .. immediateNeighborDistance]
  ]
  where
    immediateNeighborDistance = 1

toInt :: PuzzleInput -> [[ElevationMeasurement]]
toInt = map $ map digitToInt

solutionDay9Part2 = const 0
