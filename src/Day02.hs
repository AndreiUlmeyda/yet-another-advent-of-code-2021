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

-- ######### Part One #########
solutionDay2Part1 :: [String] -> Int
solutionDay2Part1 = multiplyDirections . sumDistances . map (toSubMovement . words)

data SubMovement = MkSubMovement
  { xDirection :: Int,
    yDirection :: Int
  }
  deriving (Show, Eq)

multiplyDirections :: SubMovement -> Int
multiplyDirections (MkSubMovement x y) = x * y

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

-- ######### Part Two #########
solutionDay2Part2 :: [String] -> Int
solutionDay2Part2 = uncurry (*) . sumDistancesConsideringAim (0, 0) . computeAim . firstAimZero . map (toSubMovementPlus . words)

data SubDirection = Forward | Up | Down deriving (Show, Eq)

data SubMovementPlus = MkSubMovementPlus
  { direction :: SubDirection,
    magnitude :: Int,
    aim :: Int
  }
  deriving (Show, Eq)

toSubMovementPlus :: [String] -> SubMovementPlus
toSubMovementPlus directionAndMagnitude
  | direction == "forward" = MkSubMovementPlus Forward magnitude 0
  | direction == "down" = MkSubMovementPlus Down magnitude 0
  | direction == "up" = MkSubMovementPlus Up magnitude 0
  | otherwise = MkSubMovementPlus Forward 0 0
  where
    direction = head directionAndMagnitude
    magnitude = read (head (tail directionAndMagnitude)) :: Int

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

sumDistancesConsideringAim :: (Int, Int) -> [SubMovementPlus] -> (Int, Int)
sumDistancesConsideringAim position movements
  | [] <- movements = position
  | MkSubMovementPlus Forward magnitude aim : rest <- movements = sumDistancesConsideringAim (newX, newY) rest
  | otherwise = sumDistancesConsideringAim position (tail movements)
  where
    newX = fst position + magnitude (head movements)
    newY = snd position + (magnitude (head movements) * aim (head movements))