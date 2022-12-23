module Day12
  ( solutionDay12Part1,
    solutionDay12Part2,
    generatePaths,
    Connection (MkConnection),
    mkConnection,
  )
where

import Data.Char (isUpper)
import Data.List ((\\))
import Data.List.Split
  ( splitOn,
  )
import Day04 (PuzzleInput)
import Prelude

data CaveSize = SmallCave | BigCave deriving stock (Show, Eq)

type Path = [Connection]

data Connection = MkConnection
  { caveOne :: String,
    caveTwo :: String,
    caveOneSize :: CaveSize,
    caveTwoSize :: CaveSize,
    wasVisited :: Bool
  }
  deriving stock (Show, Eq)

mkCaveSize :: String -> CaveSize
mkCaveSize caveName
  | length caveName == 1 && isUpper (head caveName) = BigCave
  | otherwise = SmallCave

mkConnection :: [String] -> Connection
mkConnection startAndEnd = MkConnection start end (mkCaveSize start) (mkCaveSize end) False
  where
    start = head startAndEnd
    end = head $ tail startAndEnd

-- ######### Part One #########
solutionDay12Part1 :: PuzzleInput -> [[Connection]]
solutionDay12Part1 = generatePaths . parseCaveConnections

generatePaths :: [Connection] -> [Path]
generatePaths connections
  | ((== 1) . length) (filter ((== "start") . caveOne) connections) = error ""
  | otherwise = []

generatePaths' :: [Connection] -> [Path]
generatePaths' [] = []
generatePaths' (connection : rest) = path : generatePaths unusedPaths
  where
    path = connection : rest
    unusedPaths = rest \\ path
    connectedToFirst = id

parseCaveConnections :: [String] -> [Connection]
parseCaveConnections = fmap (mkConnection . splitOn "-")

-- ######### Part Two #########
solutionDay12Part2 :: PuzzleInput -> Int
solutionDay12Part2 = const 2