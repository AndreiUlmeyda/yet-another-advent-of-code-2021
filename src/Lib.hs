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
    sumDistancesConsideringAim,
  )
where

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

solutionDay2Part2 :: [String] -> Int
solutionDay2Part2 = uncurry (*) . sumDistancesConsideringAim (0, 0) . computeAim . firstAimZero . map (toSubMovementPlus . words)

firstAimZero :: [SubMovementPlus] -> [SubMovementPlus]
firstAimZero movements
  | (MkSubMovementPlus direction magnitude _) : rest <- movements = MkSubMovementPlus direction magnitude 0 : rest
  | otherwise = movements

computeAim :: [SubMovementPlus] -> [SubMovementPlus]
computeAim movements
  | firstMovement : secondMovement : rest <- movements = firstMovement : computeAim (updateAimWithPreviousMovement firstMovement secondMovement : rest)
  | otherwise = movements

updateAimWithPreviousMovement :: SubMovementPlus -> SubMovementPlus -> SubMovementPlus
updateAimWithPreviousMovement firstMovement secondMovement
  | MkSubMovementPlus Down secondMagnitude secondAim <- secondMovement = MkSubMovementPlus Down secondMagnitude (aim firstMovement + secondMagnitude)
  | MkSubMovementPlus Up secondMagnitude secondAim <- secondMovement = MkSubMovementPlus Up secondMagnitude (aim firstMovement - secondMagnitude)
  | otherwise = MkSubMovementPlus (direction secondMovement) (magnitude secondMovement) (aim firstMovement)

aimDependingOnDirection :: SubMovementPlus -> SubMovementPlus
aimDependingOnDirection movement
  | MkSubMovementPlus Down magnitude aim <- movement = MkSubMovementPlus Down magnitude (aim + magnitude)
  | MkSubMovementPlus Up magnitude aim <- movement = MkSubMovementPlus Up magnitude (aim - magnitude)
  | otherwise = movement

sumDistancesConsideringAim :: (Int, Int) -> [SubMovementPlus] -> (Int, Int)
sumDistancesConsideringAim position movements
  | [] <- movements = position
  | MkSubMovementPlus Forward magnitude aim : rest <- movements = sumDistancesConsideringAim (newX, newY) rest
  | otherwise = sumDistancesConsideringAim position (tail movements)
  where
    newX = fst position + magnitude (head movements)
    newY = snd position + (magnitude (head movements) * aim (head movements))

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
