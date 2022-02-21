module Day10
  ( solutionDay10Part1,
    solutionDay10Part2,
    firstCorruptCharacter,
  )
where

import Day04 (PuzzleInput)

solutionDay10Part1 :: PuzzleInput -> [Maybe Char]
solutionDay10Part1 = map firstCorruptCharacter

firstCorruptCharacter :: String -> Maybe Char
firstCorruptCharacter = firstCorruptCharacter' []

firstCorruptCharacter' :: String -> String -> Maybe Char
firstCorruptCharacter' _ [] = Nothing
firstCorruptCharacter' openedBrackets (first : rest)
  | isOpeningCharacter first = firstCorruptCharacter' (openedBrackets ++ [first]) rest
  | isClosingCharacter first,
    null openedBrackets =
    Just first
  | isClosingCharacter first,
    last openedBrackets `isClosedBy` first =
    firstCorruptCharacter' (init openedBrackets) rest
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
solutionDay10Part2 = const 0

-- TODO avoid unsafe functions entirely
