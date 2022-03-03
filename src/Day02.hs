module Day02
  ( solutionDay2Part1,
    solutionDay2Part2,
    SubMovementPlus (MkSubMovementPlus),
    SubDirection (Forward, Up, Down),
    sumDistancesConsideringAim,
    computeAim,
    toSubMovementPlus,
  )
where

import Prelude

directionForward :: String
directionForward = "forward"

directionUp :: String
directionUp = "up"

directionDown :: String
directionDown = "down"

defaultAim :: Int
defaultAim = 0

defaultMagnitude :: Int
defaultMagnitude = 0

-- ######### Part One #########
solutionDay2Part1 :: [String] -> Int
solutionDay2Part1 = multiplyDirections . sumDistances . map (toSubMovement . words)

data SubMovement = MkSubMovement
  { xDirection :: Int,
    yDirection :: Int
  }
  deriving stock (Show, Eq)

multiplyDirections :: SubMovement -> Int
multiplyDirections (MkSubMovement x y) = x * y

sumDistances :: [SubMovement] -> SubMovement
sumDistances = foldl sumDistances' (MkSubMovement 0 0)
  where
    sumDistances' firstMovement secondMovement = MkSubMovement (xDirection firstMovement + xDirection secondMovement) (yDirection firstMovement + yDirection secondMovement)

toSubMovement :: [String] -> SubMovement
toSubMovement directionAndDistance
  | direction == directionForward = MkSubMovement distance 0
  | direction == directionDown = MkSubMovement 0 distance
  | direction == directionUp = MkSubMovement 0 (- distance)
  | otherwise = MkSubMovement 0 0
  where
    direction = head directionAndDistance
    distance = read (head (tail directionAndDistance)) :: Int

-- ######### Part Two #########
solutionDay2Part2 :: [String] -> Int
solutionDay2Part2 = uncurry (*) . sumDistancesConsideringAim startingPoint . computeAim . firstAimZero . map (toSubMovementPlus . words)

data SubDirection = Forward | Up | Down deriving stock (Show, Eq)

data SubMovementPlus = MkSubMovementPlus
  { movementDirection :: SubDirection,
    movementMagnitude :: Int,
    aim :: Int
  }
  deriving stock (Show, Eq)

toSubMovementPlus :: [String] -> SubMovementPlus
toSubMovementPlus [directionString, magnitudeString]
  | directionForward == directionString = MkSubMovementPlus Forward magnitude defaultAim
  | directionDown == directionString = MkSubMovementPlus Down magnitude defaultAim
  | directionUp == directionString = MkSubMovementPlus Up magnitude defaultAim
  where
    magnitude = read magnitudeString :: Int
toSubMovementPlus _ = MkSubMovementPlus Forward defaultMagnitude defaultAim

firstAimZero :: [SubMovementPlus] -> [SubMovementPlus]
firstAimZero movements
  | (MkSubMovementPlus direction magnitude _) : rest <- movements = MkSubMovementPlus direction magnitude 0 : rest
  | otherwise = movements

computeAim :: [SubMovementPlus] -> [SubMovementPlus]
computeAim movements
  | firstMovement : secondMovement : rest <- movements = firstMovement : computeAim (updateAimWithPreviousMovement firstMovement secondMovement : rest)
  | otherwise = movements

startingPoint :: (Int, Int)
startingPoint = (0, 0)

updateAimWithPreviousMovement :: SubMovementPlus -> SubMovementPlus -> SubMovementPlus
updateAimWithPreviousMovement firstMovement secondMovement
  | MkSubMovementPlus Down secondMagnitude _ <- secondMovement = MkSubMovementPlus Down secondMagnitude (aim firstMovement + secondMagnitude)
  | MkSubMovementPlus Up secondMagnitude _ <- secondMovement = MkSubMovementPlus Up secondMagnitude (aim firstMovement - secondMagnitude)
  | otherwise = MkSubMovementPlus (movementDirection secondMovement) (movementMagnitude secondMovement) (aim firstMovement)

sumDistancesConsideringAim :: (Int, Int) -> [SubMovementPlus] -> (Int, Int)
sumDistancesConsideringAim position movements
  | [] <- movements = position
  | MkSubMovementPlus Forward _ _ : rest <- movements = sumDistancesConsideringAim (newX, newY) rest
  | otherwise = sumDistancesConsideringAim position (tail movements)
  where
    newX = fst position + movementMagnitude (head movements)
    newY = snd position + (movementMagnitude (head movements) * aim (head movements))