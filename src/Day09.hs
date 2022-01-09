module Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
where

import Data.Array.Unboxed as A
  ( UArray,
    array,
    bounds,
    inRange,
    indices,
    (!),
  )
import Data.Char (digitToInt)
import Data.List (group, sort)
import Data.Map as M (Map, elems, fromList, lookup, mapWithKey, (!))
import Data.Maybe (fromJust)
import Day04 (PuzzleInput)
import Prelude hiding (lookup)

type ElevationMeasurement = Int

type NeighboringMeasurement = ElevationMeasurement

type CoordinatePoint = (Int, Int)

type NeighboringCoordinatePoint = CoordinatePoint

type DangerLevel = Int

type BasinIndex = Int

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
listOfNeighbors measurements coordinate = (measurements A.! coordinate, map (measurements A.!) (neighboringCoordinates validIndex coordinate))
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

solutionDay9Part2 :: PuzzleInput -> BasinIndex
solutionDay9Part2 = multiplyLargestBasinSizes . computeFlowTargets . toMap . toInt

multiplyLargestBasinSizes :: Map CoordinatePoint CoordinatePoint -> BasinIndex
multiplyLargestBasinSizes = product . take 3 . reverse . sort . map (snd . pairWithLength) . group . sort . filter (/= (-1, -1)) . elems

pairWithLength :: [CoordinatePoint] -> (CoordinatePoint, Int)
pairWithLength coordinates = (head coordinates, length coordinates)

computeFlowTargets :: Map CoordinatePoint ElevationMeasurement -> Map CoordinatePoint CoordinatePoint
computeFlowTargets measurements = mapWithKey (flowsTowards measurements) measurements

toMap :: [[ElevationMeasurement]] -> Map CoordinatePoint ElevationMeasurement
toMap measurements = fromList (zip [(a, b) | a <- [0 .. rowCount], b <- [0 .. columnCount]] (concat measurements))
  where
    (rowCount, columnCount) = (length measurements - 1, length (head measurements) - 1)

flowsTowards :: Map CoordinatePoint ElevationMeasurement -> CoordinatePoint -> ElevationMeasurement -> CoordinatePoint
flowsTowards measurements startingCoord value
  | measurements M.! startingCoord == 9 = (-1, -1) -- TODO use Maybe instead
  | any indexOfSmallerNeighbor neighbors = flowsTowards measurements (head (filter indexOfSmallerNeighbor neighbors)) (fromJust $ lookup (head (filter indexOfSmallerNeighbor neighbors)) measurements)
  | otherwise = startingCoord
  where
    neighbors = neighboringCoordinatesTwo startingCoord :: [NeighboringCoordinatePoint]
    indexOfSmallerNeighbor = isSmallerThan measurements startingCoord :: CoordinatePoint -> Bool

-- TODO use Map in both solutions, remove more or less dupicate functions like this in the process
neighboringCoordinatesTwo :: CoordinatePoint -> [NeighboringCoordinatePoint]
neighboringCoordinatesTwo (xCoord, yCoord) =
  [(xCoord, yCoord + 1), (xCoord, yCoord -1), (xCoord + 1, yCoord), (xCoord - 1, yCoord)]

isSmallerThan :: Map CoordinatePoint ElevationMeasurement -> CoordinatePoint -> CoordinatePoint -> Bool
isSmallerThan measurements coord coord2
  | Nothing <- lookup coord measurements = True
  | Nothing <- lookup coord2 measurements = False
  | otherwise = lookup coord measurements > lookup coord2 measurements
