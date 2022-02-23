module Day10
  ( solutionDay10Part1,
    solutionDay10Part2,
    firstCorruptBracket,
  )
where

import Data.List (sort)
import Data.Maybe (isNothing, mapMaybe)
import Day04 (PuzzleInput)
import Prelude

type Bracket = Char

type Line = [Bracket]

type OpenedBrackets = [Bracket]

type BracketScore = Int

type LineScore = Int

solutionDay10Part1 :: PuzzleInput -> Int
solutionDay10Part1 = sum . map scoreBracket . mapMaybe firstCorruptBracket

scoreBracket :: Bracket -> BracketScore
scoreBracket bracket
  | ')' <- bracket = 3
  | ']' <- bracket = 57
  | '}' <- bracket = 1197
  | '>' <- bracket = 25137
  | otherwise = 0

firstCorruptBracket :: Line -> Maybe Bracket
firstCorruptBracket = firstCorruptBracket' []

firstCorruptBracket' :: OpenedBrackets -> Line -> Maybe Bracket
firstCorruptBracket' _ [] = Nothing
firstCorruptBracket' openedBrackets (first : rest)
  | isOpeningBracket first = firstCorruptBracket' (first : openedBrackets) rest
  | isClosingBracket first,
    null openedBrackets =
    Just first
  | isClosingBracket first,
    head openedBrackets `isClosedBy` first =
    firstCorruptBracket' (tail openedBrackets) rest
  | otherwise = Just first

isOpeningBracket :: Bracket -> Bool
isOpeningBracket = not . isClosingBracket

isClosingBracket :: Bracket -> Bool
isClosingBracket character = character `elem` ")}]>"

isClosedBy :: Bracket -> Bracket -> Bool
isClosedBy '(' ')' = True
isClosedBy '[' ']' = True
isClosedBy '{' '}' = True
isClosedBy '<' '>' = True
isClosedBy _ _ = False

solutionDay10Part2 :: PuzzleInput -> Int
solutionDay10Part2 = median . map (scoreLine . completeLine) . filter (isNothing . firstCorruptBracket)

completeLine :: Line -> Line
completeLine = completeLine' []

completeLine' :: OpenedBrackets -> Line -> Line
completeLine' openedBrackets [] = map complementaryBracket openedBrackets
completeLine' openedBrackets (first : rest)
  | isOpeningBracket first = completeLine' (first : openedBrackets) rest
  | isClosingBracket first,
    head openedBrackets `isClosedBy` first =
    completeLine' (tail openedBrackets) rest
  | otherwise = map complementaryBracket openedBrackets -- unify the first pattern with this one

complementaryBracket :: Bracket -> Bracket
complementaryBracket '(' = ')'
complementaryBracket '{' = '}'
complementaryBracket '[' = ']'
complementaryBracket '<' = '>'
complementaryBracket _ = error "derp"

scoreLine :: Line -> LineScore
scoreLine = scoreLine' 0

scoreLine' :: LineScore -> Line -> LineScore
scoreLine' = foldl (\totalScore bracket -> totalScore * 5 + scoreBracketPartTwo bracket)

scoreBracketPartTwo :: Bracket -> BracketScore
scoreBracketPartTwo bracket
  | ')' <- bracket = 1
  | ']' <- bracket = 2
  | '}' <- bracket = 3
  | '>' <- bracket = 4
  | otherwise = 0

median :: Ord a => [a] -> a
median list = ((!! (length list `div` 2)) . sort) list

-- TODO avoid unsafe functions entirely
