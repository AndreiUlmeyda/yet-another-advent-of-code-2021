module Day10
  ( solutionDay10Part1,
    solutionDay10Part2,
    firstCorruptCharacter,
  )
where

import Data.List (sort)
import Data.Maybe (isNothing, mapMaybe)
import Day04 (PuzzleInput)
import Prelude

solutionDay10Part1 :: PuzzleInput -> Int
solutionDay10Part1 = sum . map scoreBracket . mapMaybe firstCorruptCharacter

scoreBracket :: Char -> Int
scoreBracket bracket
  | ')' <- bracket = 3
  | ']' <- bracket = 57
  | '}' <- bracket = 1197
  | '>' <- bracket = 25137
  | otherwise = 0

firstCorruptCharacter :: String -> Maybe Char
firstCorruptCharacter = firstCorruptCharacter' []

firstCorruptCharacter' :: String -> String -> Maybe Char
firstCorruptCharacter' _ [] = Nothing
firstCorruptCharacter' openedBrackets (first : rest)
  | isOpeningCharacter first = firstCorruptCharacter' (first : openedBrackets) rest
  | isClosingCharacter first,
    null openedBrackets =
    Just first
  | isClosingCharacter first,
    head openedBrackets `isClosedBy` first =
    firstCorruptCharacter' (tail openedBrackets) rest
  | otherwise = Just first

isOpeningCharacter :: Char -> Bool
isOpeningCharacter = not . isClosingCharacter

isClosingCharacter :: Char -> Bool
isClosingCharacter character = character `elem` ")}]>"

isClosedBy :: Char -> Char -> Bool
isClosedBy '(' ')' = True
isClosedBy '[' ']' = True
isClosedBy '{' '}' = True
isClosedBy '<' '>' = True
isClosedBy _ _ = False

solutionDay10Part2 :: PuzzleInput -> Int
solutionDay10Part2 = median . map (scoreLine . completeLine) . filter (isNothing . firstCorruptCharacter)

completeLine :: String -> String
completeLine = completeLine' []

completeLine' :: String -> String -> String
completeLine' openedBrackets [] = map complementaryBracket openedBrackets
completeLine' openedBrackets (first : rest)
  | isOpeningCharacter first = completeLine' (first : openedBrackets) rest
  | isClosingCharacter first,
    head openedBrackets `isClosedBy` first =
    completeLine' (tail openedBrackets) rest
  | otherwise = map complementaryBracket openedBrackets -- unify the first pattern with this one

complementaryBracket :: Char -> Char
complementaryBracket '(' = ')'
complementaryBracket '{' = '}'
complementaryBracket '[' = ']'
complementaryBracket '<' = '>'
complementaryBracket _ = error "derp"

scoreLine :: String -> Int
scoreLine = scoreLine' 0

scoreLine' :: Int -> String -> Int
scoreLine' = foldl (\totalScore bracket -> totalScore * 5 + scoreBracketPartTwo bracket)

scoreBracketPartTwo :: Char -> Int
scoreBracketPartTwo bracket
  | ')' <- bracket = 1
  | ']' <- bracket = 2
  | '}' <- bracket = 3
  | '>' <- bracket = 4
  | otherwise = 0

median :: Ord a => [a] -> a
median list = ((!! (length list `div` 2)) . sort) list

-- TODO avoid unsafe functions entirely
