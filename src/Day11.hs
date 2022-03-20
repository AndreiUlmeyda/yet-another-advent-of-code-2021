module Day11
  ( solutionDay11Part1,
    solutionDay11Part2,
    increaseOctopusEnergies,
    Octopus (MkOctopus),
  )
where

import Data.Array (Array, elems, listArray)
import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust)
import Day04 (PuzzleInput)
import Prelude

data Octopus = MkOctopus
  { energyLevel :: Int,
    numberOfFlashes :: Int
  }
  deriving stock (Show, Eq)

type OctopusArray = Array Int Octopus

initialNumberOfFlashes :: Int
initialNumberOfFlashes = 0

parseOctopus :: Int -> Maybe Octopus
parseOctopus initialEnergyLevel
  | initialEnergyLevel >= 0,
    initialEnergyLevel <= 9 =
    Just $ MkOctopus initialEnergyLevel initialNumberOfFlashes
  | otherwise = Nothing

parseInput :: PuzzleInput -> OctopusArray
parseInput puzzleInput
  | all isJust resultingOctopusList = listArray (inputSizeX, inputSizeY) $ map fromJust resultingOctopusList
  | otherwise = error "Error: The input needs to consist of digits only."
  where
    resultingOctopusList = map (parseOctopus . digitToInt) (concat puzzleInput)
    inputSizeX = length puzzleInput
    inputSizeY = length $ head puzzleInput

-- ######### Part One #########
solutionDay11Part1 :: PuzzleInput -> Int
solutionDay11Part1 = countFlashes . (!! 100) . iterate (resolveFlashes . increaseOctopusEnergies) . parseInput

increaseOctopusEnergies :: OctopusArray -> OctopusArray
increaseOctopusEnergies = fmap increaseOctopusEnergy

increaseOctopusEnergy :: Octopus -> Octopus
increaseOctopusEnergy (MkOctopus energy flashCount) = MkOctopus (energy + 1) flashCount

resolveFlashes :: OctopusArray -> OctopusArray
resolveFlashes = id

countFlashes :: OctopusArray -> Int
countFlashes = sum . elems . fmap numberOfFlashes

-- ######### Part Two #########
solutionDay11Part2 :: PuzzleInput -> Int
solutionDay11Part2 = const 0

-- instead of increasing + resetting each step I probably need to
-- first increase, then resolve flashes