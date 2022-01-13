module Day10
  ( solutionDay10Part1,
    solutionDay10Part2,
    firstCorruptCharacter,
  )
where

import Data.List (delete)
import Day04 (PuzzleInput)

solutionDay10Part1 :: PuzzleInput -> [Maybe Char]
solutionDay10Part1 = map firstCorruptCharacter

firstCorruptCharacter :: String -> Maybe Char
firstCorruptCharacter input
  | [] <- input = Nothing
  | [] <- removeMatchingBrackets input = Nothing
  | otherwise = Just $ head $ removeMatchingBrackets input

removeMatchingBrackets :: String -> String
removeMatchingBrackets [] = []
removeMatchingBrackets (firstCharacter : rest)
  | matchingCharacter firstCharacter `elem` rest = removeMatchingBrackets (matchingCharacterRemovedFrom rest)
  | otherwise = firstCharacter : removeMatchingBrackets rest
  where
    matchingCharacterRemovedFrom = delete (matchingCharacter firstCharacter)

-- firstCorruptCharacter (firstCharacter : rest)
--   | rest <- [matchingCharacter firstCharacter] =
--   | otherwise = if Nothing == firstCorruptCharacter rest then Just firstCharacter else firstCorruptCharacter rest

matchingCharacter :: Char -> Char
matchingCharacter '(' = ')'
matchingCharacter '[' = ']'
matchingCharacter '{' = '}'
matchingCharacter '<' = '>'
matchingCharacter _ = ' '

solutionDay10Part2 :: PuzzleInput -> Int
solutionDay10Part2 = const 0
