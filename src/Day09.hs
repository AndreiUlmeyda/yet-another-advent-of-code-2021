module Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
where

import Data.Array.Unboxed (UArray, array, bounds, inRange, indices, ixmap, (!))
import Data.Char (digitToInt)
import Day04 (PuzzleInput)

type ElevationMeasurement = Int

type NeighboringMeasurement = ElevationMeasurement

type CoordinatePoint = (Int, Int)

type NeighboringCoordinatePoint = CoordinatePoint

type DangerLevel = Int

solutionDay9Part1 :: PuzzleInput -> Int
solutionDay9Part1 = sum . computeDangerLevel . filter isMinimum . pairWithListOfNeighbors . toArray . toInt

toInt :: PuzzleInput -> [[ElevationMeasurement]]
toInt = map $ map digitToInt

toArray :: [[ElevationMeasurement]] -> UArray CoordinatePoint ElevationMeasurement
toArray measurements = array arrayBounds (zip [(a, b) | a <- [0 .. rowCount], b <- [0 .. columnCount]] (concat measurements))
  where
    (rowCount, columnCount) = (length measurements - 1, length (head measurements) - 1)
    arrayBounds = ((0, 0), (rowCount, columnCount))

pairWithListOfNeighbors :: UArray CoordinatePoint ElevationMeasurement -> [(ElevationMeasurement, [NeighboringMeasurement])]
pairWithListOfNeighbors measurements = map (listOfNeighbors measurements) (indices measurements)

listOfNeighbors :: UArray CoordinatePoint ElevationMeasurement -> CoordinatePoint -> (ElevationMeasurement, [NeighboringMeasurement])
listOfNeighbors measurements coordinate = (measurements ! coordinate, map (measurements !) (neighboringCoordinates validIndex coordinate))
  where
    validIndex = inRange $ bounds measurements

neighboringCoordinates :: (NeighboringCoordinatePoint -> Bool) -> CoordinatePoint -> [NeighboringCoordinatePoint]
neighboringCoordinates isValidIndex (xCoord, yCoord) =
  [ (x, y)
    | neighboringRows <- [- immediateNeighborDistance .. immediateNeighborDistance],
      let x = xCoord + neighboringRows,
      neighboringColumns <- [- immediateNeighborDistance .. immediateNeighborDistance],
      let y = yCoord + neighboringColumns,
      isValidIndex (x, y)
  ]
  where
    immediateNeighborDistance = 1

isMinimum :: (ElevationMeasurement, [NeighboringMeasurement]) -> Bool
isMinimum (measurement, neighboringMeasurements) = measurement == minimum neighboringMeasurements

computeDangerLevel :: [(ElevationMeasurement, a)] -> [DangerLevel]
computeDangerLevel = map ((+) 1 . fst)

solutionDay9Part2 :: PuzzleInput -> CoordinatePoint
solutionDay9Part2 = flowsTowards (0, 0) . toArray . toInt

flowsTowards :: CoordinatePoint -> UArray CoordinatePoint ElevationMeasurement -> CoordinatePoint
flowsTowards startingCoord measurements
  | any indexOfSmallerNeighbor neighbors = flowsTowards (head (filter indexOfSmallerNeighbor neighbors)) measurements -- head $ filter indexOfSmallerNeighbor neighbors
  | otherwise = startingCoord
  where
    neighbors = neighboringCoordinates isValidIndex startingCoord
    indexOfSmallerNeighbor = isSmallerThan measurements startingCoord :: CoordinatePoint -> Bool
    isValidIndex = inRange $ bounds measurements

-- flowsTowards' coord measurements
--   |

isSmallerThan :: UArray CoordinatePoint ElevationMeasurement -> CoordinatePoint -> CoordinatePoint -> Bool
isSmallerThan measurements coord coord2 = measurements ! coord > measurements ! coord2
