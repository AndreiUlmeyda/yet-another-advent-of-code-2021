module Day10
  ( solutionDay10Part1,
    solutionDay10Part2,
    firstCorruptBracket,
  )
where

import Data.List (find, sort)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Day04 (PuzzleInput)
import Prelude

type BracketSymbol = Char

type Line = [BracketSymbol]

type OpenedBrackets = [BracketSymbol]

type BracketScore = Int

type LineScore = Int

data Bracket = MkBracket
  { openingSymbol :: Char,
    closingSymbol :: Char,
    scorePartOne :: Int,
    scorePartTwo :: Int
  }

bracketDefinitions :: [Bracket]
bracketDefinitions =
  [ MkBracket '(' ')' 3 1,
    MkBracket '[' ']' 57 2,
    MkBracket '{' '}' 1197 3,
    MkBracket '<' '>' 25137 4
  ]

defaultScore :: BracketScore
defaultScore = 0

-- ######### Part One #########
solutionDay10Part1 :: PuzzleInput -> Int
solutionDay10Part1 = sum . map scoreBracket . mapMaybe firstCorruptBracket

firstCorruptBracket :: Line -> Maybe BracketSymbol
firstCorruptBracket = firstCorruptBracket' []

firstCorruptBracket' :: OpenedBrackets -> Line -> Maybe BracketSymbol
firstCorruptBracket' _ [] = Nothing
firstCorruptBracket' openedBrackets (first : rest)
  | isOpeningBracket first = firstCorruptBracket' (first : openedBrackets) rest
  | isClosingBracket first,
    Nothing <- listToMaybe openedBrackets =
    Just first
  | isClosingBracket first,
    Just lastOpenedBracket <- listToMaybe openedBrackets,
    lastOpenedBracket `isClosedBy` first =
    firstCorruptBracket' (tail openedBrackets) rest
  | otherwise = Just first

scoreBracket :: BracketSymbol -> BracketScore
scoreBracket symbol
  | Nothing <- firstDefinitionMatchingClosingBracket symbol = defaultScore
  | Just bracketDefinition <- firstDefinitionMatchingClosingBracket symbol = scorePartOne bracketDefinition

firstDefinitionMatchingClosingBracket :: Char -> Maybe Bracket
firstDefinitionMatchingClosingBracket symbol = find (\bracket -> closingSymbol bracket == symbol) bracketDefinitions

isOpeningBracket :: BracketSymbol -> Bool
isOpeningBracket = not . isClosingBracket

isClosingBracket :: BracketSymbol -> Bool
isClosingBracket symbol
  | Nothing <- firstDefinitionMatchingClosingBracket symbol = False
  | otherwise = True

isClosedBy :: BracketSymbol -> BracketSymbol -> Bool
isClosedBy firstSymbol potentiallyClosingSymbol
  | Nothing <- matchingBracketDefinition = False
  | Just bracketDefinition <- matchingBracketDefinition = firstSymbol == openingSymbol bracketDefinition
  where
    matchingBracketDefinition = firstDefinitionMatchingClosingBracket potentiallyClosingSymbol

-- ######### Part Two #########
solutionDay10Part2 :: PuzzleInput -> Int
solutionDay10Part2 = median . map (scoreLine . completeLine) . incompleteLines

incompleteLines :: [Line] -> [Line]
incompleteLines = filter (isNothing . firstCorruptBracket)

completeLine :: Line -> Line
completeLine = completeLine' []

completeLine' :: OpenedBrackets -> Line -> Line
completeLine' openedBrackets [] = mapMaybe complementaryBracket openedBrackets
completeLine' openedBrackets (first : rest)
  | isOpeningBracket first = completeLine' (first : openedBrackets) rest
  | isClosingBracket first,
    Just lastOpenedBracket <- listToMaybe openedBrackets,
    lastOpenedBracket `isClosedBy` first =
    completeLine' (tail openedBrackets) rest
  | otherwise = mapMaybe complementaryBracket openedBrackets -- unify the first pattern with this one

complementaryBracket :: BracketSymbol -> Maybe BracketSymbol
complementaryBracket symbol
  | Nothing <- firstDefinitionMatchingOpeningBracket = Nothing
  | Just bracketDefinition <- firstDefinitionMatchingOpeningBracket = Just $ closingSymbol bracketDefinition
  where
    firstDefinitionMatchingOpeningBracket = find (\bracket -> openingSymbol bracket == symbol) bracketDefinitions

scoreLine :: Line -> LineScore
scoreLine = scoreLine' 0

scoreLine' :: LineScore -> Line -> LineScore
scoreLine' = foldl (\totalScore bracket -> totalScore * 5 + scoreBracketPartTwo bracket)

scoreBracketPartTwo :: BracketSymbol -> BracketScore
scoreBracketPartTwo symbol
  | Nothing <- firstDefinitionMatchingClosingBracket symbol = defaultScore
  | Just bracketDefinition <- firstDefinitionMatchingClosingBracket symbol = scorePartTwo bracketDefinition

median :: Ord a => [a] -> a
median list = ((!! (length list `div` 2)) . sort) list

-- TODO avoid unsafe functions entirely ... in progress
