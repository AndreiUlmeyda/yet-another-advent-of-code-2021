module Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
where

import Data.Array.Unboxed
  ( UArray,
    array,
    bounds,
    inRange,
    indices,
    (!),
  )
import Data.Char (digitToInt)
import Data.List (group, sort)
import Data.Map (Map, elems, fromList, lookup, mapWithKey, (!))
import Data.Maybe (fromJust)
import Day04 (PuzzleInput)
import GHC.Generics (UAddr)

type ElevationMeasurement = Int

type NeighboringMeasurement = ElevationMeasurement

type CoordinatePoint = (Int, Int)

type NeighboringCoordinatePoint = CoordinatePoint

type DangerLevel = Int

solutionDay9Part1 :: PuzzleInput -> Int
solutionDay9Part1 = sum . computeDangerLevel . filter isMinimum . pairWithListOfNeighbors . toArray . toInt

toInt :: PuzzleInput -> [[ElevationMeasurement]]
toInt = Prelude.map $ Prelude.map digitToInt

toArray :: [[ElevationMeasurement]] -> UArray CoordinatePoint ElevationMeasurement
toArray measurements = array arrayBounds (zip [(a, b) | a <- [0 .. rowCount], b <- [0 .. columnCount]] (concat measurements))
  where
    (rowCount, columnCount) = (length measurements - 1, length (head measurements) - 1)
    arrayBounds = ((0, 0), (rowCount, columnCount))

pairWithListOfNeighbors :: UArray CoordinatePoint ElevationMeasurement -> [(ElevationMeasurement, [NeighboringMeasurement])]
pairWithListOfNeighbors measurements = Prelude.map (listOfNeighbors measurements) (indices measurements)

listOfNeighbors :: UArray CoordinatePoint ElevationMeasurement -> CoordinatePoint -> (ElevationMeasurement, [NeighboringMeasurement])
listOfNeighbors measurements coordinate = (measurements Data.Array.Unboxed.! coordinate, Prelude.map (measurements Data.Array.Unboxed.!) (neighboringCoordinates validIndex coordinate))
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
computeDangerLevel = Prelude.map ((+) 1 . fst)

solutionDay9Part2 :: PuzzleInput -> Int
solutionDay9Part2 = product . take 3 . reverse . sort . map (snd . pairWithLength) . group . sort . filter (/= (-1, -1)) . elems . derp . toMap . toInt -- coords (y, x)

pairWithLength :: [a] -> (a, Int)
pairWithLength asd = (head asd, length asd) -- TODO should be present in another file, move to Util

-- TODO give it a name
derp :: Map CoordinatePoint ElevationMeasurement -> Map CoordinatePoint CoordinatePoint
derp measurements = Data.Map.mapWithKey (flowsTowards measurements) measurements

toMap :: [[ElevationMeasurement]] -> Map CoordinatePoint ElevationMeasurement
toMap measurements = fromList (zip [(a, b) | a <- [0 .. rowCount], b <- [0 .. columnCount]] (concat measurements))
  where
    (rowCount, columnCount) = (length measurements - 1, length (head measurements) - 1)

flowsTowards :: Map CoordinatePoint ElevationMeasurement -> CoordinatePoint -> ElevationMeasurement -> CoordinatePoint
flowsTowards measurements startingCoord value
  | measurements Data.Map.! startingCoord == 9 = (-1, -1)
  | any indexOfSmallerNeighbor neighbors = flowsTowards measurements (head (filter indexOfSmallerNeighbor neighbors)) (fromJust $ Data.Map.lookup (head (filter indexOfSmallerNeighbor neighbors)) measurements)
  | otherwise = startingCoord
  where
    neighbors = neighboringCoordinatesTwo startingCoord :: [NeighboringCoordinatePoint]
    indexOfSmallerNeighbor = isSmallerThan measurements startingCoord :: CoordinatePoint -> Bool

neighboringCoordinatesTwo :: CoordinatePoint -> [NeighboringCoordinatePoint]
neighboringCoordinatesTwo (xCoord, yCoord) =
  [(xCoord, yCoord + 1), (xCoord, yCoord -1), (xCoord + 1, yCoord), (xCoord - 1, yCoord)]

isSmallerThan :: Map CoordinatePoint ElevationMeasurement -> CoordinatePoint -> CoordinatePoint -> Bool
isSmallerThan measurements coord coord2
  | Nothing <- Data.Map.lookup coord measurements = True
  | Nothing <- Data.Map.lookup coord2 measurements = False
  | otherwise = Data.Map.lookup coord measurements > Data.Map.lookup coord2 measurements

-- TODO clean up unused imports
-- TODO clean up unused dependencies
-- TODO clean up unused functions
