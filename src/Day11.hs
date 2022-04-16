module Day11
  ( solutionDay11Part1,
    solutionDay11Part2,
    increaseOctopusEnergies,
    Octopus (MkOctopus),
    resolveFlashes,
  )
where

import Data.Char (digitToInt)
import Data.Map as M
  ( Map,
    adjust,
    elemAt,
    elems,
    filter,
    fromList,
    update,
  )
import Data.Maybe (fromJust, isJust)
import Day04 (PuzzleInput)
import Prelude

data Octopus = MkOctopus
  { energyLevel :: Int,
    numberOfFlashes :: Int,
    didFlashThisStep :: Bool
  }
  deriving stock (Eq)

-- instance Show Octopus where
--   show octopus = (show . energyLevel) octopus ++ ":" ++ (show . numberOfFlashes) octopus

instance Show Octopus where
  show = show . energyLevel

type OctopusArray = Map (Int, Int) Octopus

type CoordinatePoint = (Int, Int)

type NeighboringCoordinatePoint = CoordinatePoint

zeroInitialFlashCount :: Int
zeroInitialFlashCount = 0

zeroEnergy :: Int
zeroEnergy = 0

flashingThreshold :: Int
flashingThreshold = 9

initialFlashedStatus :: Bool
initialFlashedStatus = False

parseOctopus :: Int -> Maybe Octopus
parseOctopus energyLevelFromInput
  | energyLevelFromInput >= 0,
    energyLevelFromInput <= 9 =
    Just $ MkOctopus energyLevelFromInput zeroInitialFlashCount initialFlashedStatus
  | otherwise = Nothing

parseInput :: PuzzleInput -> OctopusArray
parseInput puzzleInput
  | all isJust resultingOctopusList = fromList (zip [(a, b) | a <- [0 .. inputSizeX], b <- [0 .. inputSizeY]] (map fromJust resultingOctopusList))
  | otherwise = error "Error: The input needs to consist of digits only."
  where
    resultingOctopusList = map (parseOctopus . digitToInt) (concat puzzleInput)
    inputSizeX = length puzzleInput - 1
    inputSizeY = length (head puzzleInput) - 1

neighboringCoordinates :: CoordinatePoint -> [NeighboringCoordinatePoint]
neighboringCoordinates (xCoord, yCoord) =
  [ (xCoord, yCoord + 1),
    (xCoord, yCoord -1),
    (xCoord + 1, yCoord),
    (xCoord - 1, yCoord),
    (xCoord + 1, yCoord + 1),
    (xCoord - 1, yCoord + 1),
    (xCoord + 1, yCoord - 1),
    (xCoord - 1, yCoord - 1)
  ]

incrementCoordinates :: OctopusArray -> [CoordinatePoint] -> OctopusArray
incrementCoordinates octopuses [] = octopuses
incrementCoordinates octopuses (coord : rest) = incrementCoordinates incrementedCoordinate rest
  where
    incrementedCoordinate = adjust incrementEnergy coord octopuses

incrementEnergy :: Octopus -> Octopus
incrementEnergy (MkOctopus energy numFlashes didFlash) = MkOctopus (energy + 1) numFlashes didFlash

-- ######### Part One #########
-- solutionDay11Part1 :: PuzzleInput -> Int
solutionDay11Part1 = countFlashes . (!! 100) . iterate (resolveFlashes . increaseOctopusEnergies) . parseInput

increaseOctopusEnergies :: OctopusArray -> OctopusArray
increaseOctopusEnergies = fmap increaseOctopusEnergy

increaseOctopusEnergy :: Octopus -> Octopus
increaseOctopusEnergy (MkOctopus energy flashCount flashStatus) = MkOctopus (energy + 1) flashCount flashStatus

resolveFlashes :: OctopusArray -> OctopusArray
resolveFlashes = fmap resetEnergy . flashAndIncreaseEnergies

flashAndIncreaseEnergies :: OctopusArray -> OctopusArray
flashAndIncreaseEnergies octopuses
  | not (any aboveThresholdButNotFlashed octopuses) = octopuses
  | otherwise = flashAndIncreaseEnergies $ flashFirstAboveThreshold octopuses
  where
    aboveThresholdButNotFlashed octopus = energyLevel octopus > flashingThreshold && not (didFlashThisStep octopus)

flashFirstAboveThreshold :: OctopusArray -> OctopusArray
flashFirstAboveThreshold octopuses = markFlashed . incrementCoordinates octopuses $ neighboringCoordinates $ fst (elemAt 0 (M.filter aboveThresholdButNotFlashed octopuses))
  where
    markFlashed = update derp (fst (elemAt 0 (M.filter aboveThresholdButNotFlashed octopuses)))
    derp (MkOctopus energy flashCount _) = Just $ MkOctopus energy flashCount True -- increase flashcount only at the end
    aboveThresholdButNotFlashed octopus = energyLevel octopus > flashingThreshold && not (didFlashThisStep octopus)

resetEnergy :: Octopus -> Octopus
resetEnergy (MkOctopus energy flashCount flashStatus)
  | energy > flashingThreshold = MkOctopus zeroEnergy (flashCount + 1) False
  | otherwise = MkOctopus energy flashCount flashStatus

countFlashes :: OctopusArray -> Int
countFlashes = sum . elems . fmap numberOfFlashes

-- ######### Part Two #########
solutionDay11Part2 :: PuzzleInput -> Int
solutionDay11Part2 = const 0

-- instead of increasing + resetting each step I probably need to
-- first increase, then resolve flashes

-- resolve flashes in multiple steps
-- 1. increment neighbors when origin is above threshold
--    mark origin as having flashed
--    repeat until no octopus is left which is above threshold and is not marked as having flashed