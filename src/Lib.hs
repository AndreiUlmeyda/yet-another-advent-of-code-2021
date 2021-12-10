module Lib
  ( solutionDay1Part1,
    solutionDay1Part2,
    solutionDay2Part1,
    solutionDay2Part2,
    computeAim,
    SubmarineMovement (MkSubmarineMovement),
  )
where

import Control.Exception (MaskingState (MaskedUninterruptible))
import Data.Bifunctor (bimap)

data SubmarineMovement = MkSubmarineMovement
  { xDirection :: Int,
    yDirection :: Int,
    aim :: Int
  }
  deriving (Show, Eq)

solutionDay1Part1 :: [String] -> Int
solutionDay1Part1 = countIncreases . map toInt

solutionDay1Part2 :: [String] -> Int
solutionDay1Part2 = countIncreases . slidingWindowSumLengthThree . map toInt

solutionDay2Part1 :: [String] -> Int
solutionDay2Part1 = multiplyDirections . sumDistances . map (toDirections . words)

multiplyDirections :: SubmarineMovement -> Int
multiplyDirections (MkSubmarineMovement x y aim) = x * y

solutionDay2Part2 :: [String] -> Int
solutionDay2Part2 = multiplyDirections . sumDistancesConsideringAim . map (toDirections . words)

computeAim :: SubmarineMovement -> SubmarineMovement
computeAim (MkSubmarineMovement x y aim)
  | y > 0 = MkSubmarineMovement x y (aim + 5)
  | y < 0 = MkSubmarineMovement x y (aim - 3)
  | otherwise = MkSubmarineMovement x y aim

sumDistancesConsideringAim :: [SubmarineMovement] -> SubmarineMovement
sumDistancesConsideringAim = foldl sumDistancesConsideringAim' (MkSubmarineMovement 0 0 0)
  where
    sumDistancesConsideringAim' firstMovement secondMovement = MkSubmarineMovement (xDirection firstMovement + xDirection secondMovement) (yDirection firstMovement + yDirection secondMovement) (aim firstMovement + aim secondMovement)

sumDistances :: [SubmarineMovement] -> SubmarineMovement
sumDistances = foldl sumDistances' (MkSubmarineMovement 0 0 0)
  where
    sumDistances' firstMovement secondMovement = MkSubmarineMovement (xDirection firstMovement + xDirection secondMovement) (yDirection firstMovement + yDirection secondMovement) 0

toDirections :: [String] -> SubmarineMovement
toDirections directionAndDistance
  | direction == "forward" = MkSubmarineMovement distance 0 0
  | direction == "down" = MkSubmarineMovement 0 distance 0
  | direction == "up" = MkSubmarineMovement 0 (- distance) 0
  | otherwise = MkSubmarineMovement 0 0 0
  where
    direction = head directionAndDistance
    distance = read (head (tail directionAndDistance))

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
