module Day11
  ( solutionDay11Part1,
    solutionDay11Part2,
    Octopus (MkOctopus),
    flashAndIncreaseEnergies,
    parseInput,
  )
where

import Data.Char (digitToInt)
import Data.Map as M
  ( Map,
    adjust,
    elemAt,
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

neighboringCoordinates :: CoordinatePoint -> [NeighboringCoordinatePoint]
neighboringCoordinates (xCoord, yCoord) = [(xCoord + x, yCoord + y) | x <- [-1 .. 1], y <- [-1 .. 1], not (x == 0 && y == 0)]

incrementCoordinates :: [CoordinatePoint] -> OctopusArray -> OctopusArray
incrementCoordinates [] octopuses = octopuses
incrementCoordinates (coord : rest) octopuses = incrementCoordinates rest incrementedCoordinate
  where
    incrementedCoordinate = adjust incrementEnergy coord octopuses

incrementEnergy :: Octopus -> Octopus
incrementEnergy (MkOctopus energy numFlashes didFlash) = MkOctopus (energy + 1) numFlashes didFlash

-- ######### Part One #########
solutionDay11Part1 :: PuzzleInput -> Int
solutionDay11Part1 = sumUpFlashes . (!! targetStep) . simulateOctopuses . parseInput

targetStep :: Int
targetStep = 100

parseInput :: PuzzleInput -> OctopusArray
parseInput puzzleInput
  | all isJust resultingOctopusList =
    fromList
      ( zip
          [(xCoords, yCoords) | xCoords <- [1 .. inputSizeX], yCoords <- [1 .. inputSizeY]]
          (map fromJust resultingOctopusList)
      )
  | otherwise = error "Error: The input needs to consist of digits only."
  where
    resultingOctopusList = map (parseOctopus . digitToInt) (concat puzzleInput)
    inputSizeX = length puzzleInput
    inputSizeY = length (head puzzleInput)

parseOctopus :: Int -> Maybe Octopus
parseOctopus energyLevelFromInput
  | energyLevelFromInput >= 0,
    energyLevelFromInput <= 9 =
    Just $ MkOctopus energyLevelFromInput zeroInitialFlashCount initialFlashedStatus
  | otherwise = Nothing

simulateOctopuses :: OctopusArray -> [OctopusArray]
simulateOctopuses = iterate (fmap handleFlashedOctopus . flashAndIncreaseEnergies . fmap incrementOctopusEnergy)

incrementOctopusEnergy :: Octopus -> Octopus
incrementOctopusEnergy (MkOctopus energy flashCount flashStatus) = MkOctopus (energy + 1) flashCount flashStatus

flashAndIncreaseEnergies :: OctopusArray -> OctopusArray
flashAndIncreaseEnergies octopuses
  | not (any aboveThresholdButNotFlashed octopuses) = octopuses
  | otherwise = flashAndIncreaseEnergies (flashFirstAboveThreshold octopuses)

aboveThresholdButNotFlashed :: Octopus -> Bool
aboveThresholdButNotFlashed octopus = energyLevel octopus > flashingThreshold && not (didFlashThisStep octopus)

flashFirstAboveThreshold :: OctopusArray -> OctopusArray
flashFirstAboveThreshold octopuses =
  ( markFlashedAtCoordinate firstUnflashed . incrementCoordinates (neighboringCoordinates firstUnflashed)
  )
    octopuses
  where
    firstUnflashed = firstUnFlashedCoordinate octopuses

markFlashedAtCoordinate :: CoordinatePoint -> OctopusArray -> OctopusArray
markFlashedAtCoordinate = update (Just . markFlashed)

firstUnFlashedCoordinate :: OctopusArray -> CoordinatePoint
firstUnFlashedCoordinate octopuses = fst (elemAt 0 (M.filter aboveThresholdButNotFlashed octopuses))

handleFlashedOctopus :: Octopus -> Octopus
handleFlashedOctopus octopus
  | energyLevel octopus > flashingThreshold = (resetEnergy . resetFlashStatus . incrementFlashCount) octopus
  | otherwise = octopus

resetEnergy :: Octopus -> Octopus
resetEnergy (MkOctopus _ flashCount flashStatus) = MkOctopus zeroEnergy flashCount flashStatus

incrementFlashCount :: Octopus -> Octopus
incrementFlashCount (MkOctopus energy flashCount flashStatus) = MkOctopus energy (flashCount + 1) flashStatus

markFlashed :: Octopus -> Octopus
markFlashed (MkOctopus energy flashCount _) = MkOctopus energy flashCount True

resetFlashStatus :: Octopus -> Octopus
resetFlashStatus (MkOctopus energy flashCount _) = MkOctopus energy flashCount False

sumUpFlashes :: OctopusArray -> Int
sumUpFlashes = sum . fmap numberOfFlashes

-- ######### Part Two #########
solutionDay11Part2 :: PuzzleInput -> Int
solutionDay11Part2 = length . takeWhile nonSimultaneousFlashes . simulateOctopuses . parseInput

nonSimultaneousFlashes :: OctopusArray -> Bool
nonSimultaneousFlashes = not . null . M.filter nonZeroEnergy

nonZeroEnergy :: Octopus -> Bool
nonZeroEnergy = (/=) zeroEnergy . energyLevel

-- instead of increasing + resetting each step I probably need to
-- first increase, then resolve flashes

-- resolve flashes in multiple steps
-- 1. increment neighbors when origin is above threshold
--    mark origin as having flashed
--    repeat until no octopus is left which is above threshold and is not marked as having flashed