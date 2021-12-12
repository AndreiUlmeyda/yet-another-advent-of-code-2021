module Lib
  ( solutionDay1Part1,
    solutionDay1Part2,
    solutionDay2Part1,
    solutionDay2Part2,
    computeAim,
    SubMovement (MkSubMovement),
    SubMovementPlus (MkSubMovementPlus),
    SubDirection (Forward, Down, Up),
    toSubMovementPlus,
  )
where

import Data.Bifunctor (bimap)

data SubMovement = MkSubMovement
  { xDirection :: Int,
    yDirection :: Int
  }
  deriving (Show, Eq)

data SubMovementPlus = MkSubMovementPlus
  { direction :: SubDirection,
    magnitude :: Int,
    aim :: Int
  }
  deriving (Show, Eq)

data SubDirection = Forward | Up | Down deriving (Show, Eq)

solutionDay1Part1 :: [String] -> Int
solutionDay1Part1 = countIncreases . map toInt

solutionDay1Part2 :: [String] -> Int
solutionDay1Part2 = countIncreases . slidingWindowSumLengthThree . map toInt

solutionDay2Part1 :: [String] -> Int
solutionDay2Part1 = multiplyDirections . sumDistances . map (toSubMovement . words)

multiplyDirections :: SubMovement -> Int
multiplyDirections (MkSubMovement x y) = x * y

solutionDay2Part2 :: [String] -> [SubMovementPlus]
solutionDay2Part2 = computeAim . map (toSubMovementPlus . words)

-- solutionDay2Part2 = multiplyDirections . sumDistancesConsideringAim . map (toSubMovement . words)

computeAim :: [SubMovementPlus] -> [SubMovementPlus]
computeAim [] = []
computeAim [MkSubMovementPlus direction magnitude _] = [MkSubMovementPlus direction magnitude 0]
computeAim _ = []

-- computeAim (MkSubMovement x y aim)
--   | y > 0 = MkSubMovement x y (aim + 5)
--   | y < 0 = MkSubMovement x y (aim - 3)
--   | otherwise = MkSubMovement x y aim

sumDistancesConsideringAim :: [SubMovement] -> SubMovement
sumDistancesConsideringAim = foldr sumDistancesConsideringAim' (MkSubMovement 0 0)
  where
    sumDistancesConsideringAim' firstMovement secondMovement = MkSubMovement (xDirection firstMovement + xDirection secondMovement) (yDirection firstMovement + yDirection secondMovement)

sumDistances :: [SubMovement] -> SubMovement
sumDistances = foldl sumDistances' (MkSubMovement 0 0)
  where
    sumDistances' firstMovement secondMovement = MkSubMovement (xDirection firstMovement + xDirection secondMovement) (yDirection firstMovement + yDirection secondMovement)

toSubMovement :: [String] -> SubMovement
toSubMovement directionAndDistance
  | direction == "forward" = MkSubMovement distance 0
  | direction == "down" = MkSubMovement 0 distance
  | direction == "up" = MkSubMovement 0 (- distance)
  | otherwise = MkSubMovement 0 0
  where
    direction = head directionAndDistance
    distance = read (head (tail directionAndDistance))

toSubMovementPlus :: [String] -> SubMovementPlus
toSubMovementPlus directionAndMagnitude
  | direction == "forward" = MkSubMovementPlus Forward magnitude 0
  | direction == "down" = MkSubMovementPlus Down magnitude 0
  | direction == "up" = MkSubMovementPlus Up magnitude 0
  | otherwise = MkSubMovementPlus Forward 0 0
  where
    direction = head directionAndMagnitude
    magnitude = read (head (tail directionAndMagnitude)) :: Int

toInt :: String -> Int
toInt = read

countIncreases :: [Int] -> Int
countIncreases = length . filter (> 0) . listOfDifferences

listOfDifferences :: [Int] -> [Int]
listOfDifferences input
  | first : second : rest <- input = (second - first) : listOfDifferences (second : rest)
  | otherwise = []

slidingWindowSumLengthThree :: [Int] -> [Int]
slidingWindowSumLengthThree input
  | first : second : third : rest <- input = first + second + third : slidingWindowSumLengthThree (second : third : rest)
  | otherwise = []
