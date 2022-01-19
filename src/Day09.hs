module Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
where

import Data.Char (digitToInt)
import Data.List (group, sort)
import Data.Map
  ( Map,
    elems,
    fromList,
    keys,
    lookup,
    mapMaybeWithKey,
    member,
    (!),
  )
import Data.Maybe (fromJust)
import Day04 (PuzzleInput)
import Prelude hiding (lookup)

type ElevationMeasurement = Int

type NeighboringMeasurement = ElevationMeasurement

type CoordinatePoint = (Int, Int)

type NeighboringCoordinatePoint = CoordinatePoint

type DangerLevel = Int

type BasinIndex = Int

solutionDay9Part1 :: PuzzleInput -> DangerLevel
solutionDay9Part1 = sum . computeDangerLevels . map fst . filter isMinimum . pairWithNeighboringMeasurements . toMap . toInt

toInt :: PuzzleInput -> [[ElevationMeasurement]]
toInt = map $ map digitToInt

pairWithNeighboringMeasurements :: Map CoordinatePoint ElevationMeasurement -> [(ElevationMeasurement, [NeighboringMeasurement])]
pairWithNeighboringMeasurements measurements = map (listOfNeighbors measurements) (keys measurements)

listOfNeighbors :: Map CoordinatePoint ElevationMeasurement -> CoordinatePoint -> (ElevationMeasurement, [NeighboringMeasurement])
listOfNeighbors measurements coordinate = (currentElevation, neighboringElevations)
  where
    currentElevation = measurements ! coordinate :: ElevationMeasurement
    neighboringElevations = (map (measurements !) . filter (`member` measurements)) (neighboringCoordinates coordinate)

isMinimum :: (ElevationMeasurement, [NeighboringMeasurement]) -> Bool
isMinimum (measurement, neighboringMeasurements) = measurement < minimum neighboringMeasurements

computeDangerLevels :: [ElevationMeasurement] -> [DangerLevel]
computeDangerLevels = map (1 +)

solutionDay9Part2 :: PuzzleInput -> BasinIndex
solutionDay9Part2 = multiplyLargestBasinSizes . computeFlowTargets . toMap . toInt

multiplyLargestBasinSizes :: Map CoordinatePoint CoordinatePoint -> BasinIndex
multiplyLargestBasinSizes = product . take 3 . reverse . sort . map (snd . pairWithLength) . group . sort . elems

pairWithLength :: [CoordinatePoint] -> (CoordinatePoint, Int)
pairWithLength coordinates = (head coordinates, length coordinates)

computeFlowTargets :: Map CoordinatePoint ElevationMeasurement -> Map CoordinatePoint CoordinatePoint
computeFlowTargets measurements = mapMaybeWithKey (flowsTowards measurements) measurements

toMap :: [[ElevationMeasurement]] -> Map CoordinatePoint ElevationMeasurement
toMap measurements = fromList (zip [(a, b) | a <- [0 .. rowCount], b <- [0 .. columnCount]] (concat measurements))
  where
    (rowCount, columnCount) = (length measurements - 1, length (head measurements) - 1) :: (Int, Int)

flowsTowards :: Map CoordinatePoint ElevationMeasurement -> CoordinatePoint -> ElevationMeasurement -> Maybe CoordinatePoint
flowsTowards measurements startingCoord _
  | measurements ! startingCoord == 9 = Nothing
  | any isSmallerNeighbor neighbors = flowsTowards measurements (head smallerNeighbors) (fromJust $ lookup (head smallerNeighbors) measurements)
  | otherwise = Just startingCoord
  where
    neighbors = neighboringCoordinates startingCoord :: [NeighboringCoordinatePoint]
    isSmallerNeighbor = isSmallerThan measurements startingCoord :: NeighboringCoordinatePoint -> Bool
    smallerNeighbors = filter isSmallerNeighbor neighbors :: [NeighboringCoordinatePoint]

neighboringCoordinates :: CoordinatePoint -> [NeighboringCoordinatePoint]
neighboringCoordinates (xCoord, yCoord) =
  [(xCoord, yCoord + 1), (xCoord, yCoord -1), (xCoord + 1, yCoord), (xCoord - 1, yCoord)]

-- TODO this function needs to be named/expressed more clearly
isSmallerThan :: Map CoordinatePoint ElevationMeasurement -> CoordinatePoint -> CoordinatePoint -> Bool
isSmallerThan measurements coord coord2
  | Nothing <- lookup coord measurements = False
  | Nothing <- lookup coord2 measurements = False
  | otherwise = lookup coord measurements > lookup coord2 measurements
