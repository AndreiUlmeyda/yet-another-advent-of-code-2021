module Day10
  ( solutionDay10Part1,
    solutionDay10Part2,
    firstCorruptCharacter,
    parseChunks,
    Chunk (MkChunk, MkInCompleteChunk),
    contentOfFirstBracket,
    upUntilNthCharacter,
  )
where

import Data.List (delete, stripPrefix)
import Data.Maybe (fromJust)
import Day04 (PuzzleInput)
import Prelude

data Chunk
  = MkChunk
      { openingCharacter :: Char,
        innerChunks :: [Chunk]
      }
  | MkInCompleteChunk
      { openingCharacter :: Char,
        innerChunks :: [Chunk]
      }
  deriving stock (Show, Eq)

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
  | fromJust (matchingCharacter firstCharacter) `elem` rest =
    if
        | [] <- removeMatchingBrackets innerBracket -> removeMatchingBrackets (firstMatchingCharacterRemovedFrom rest)
        | otherwise -> removeMatchingBrackets innerBracket ++ removeMatchingBrackets (firstMatchingCharacterRemovedFrom rest) -- the tricky case, there is a first character AND the inner brackets are not matched ->
  | otherwise = firstCharacter : removeMatchingBrackets rest
  where
    firstMatchingCharacterRemovedFrom = delete (fromJust (matchingCharacter firstCharacter))
    innerBracket = takeWhile (/= fromJust (matchingCharacter firstCharacter)) rest -- the definition of inner bracket is wrong, if another bracket opens then the corresponding closing bracket is not the first but the second one

parseChunks :: String -> [Chunk]
parseChunks stringOfBrackets
  | [] <- stringOfBrackets = []
  | [a] <- stringOfBrackets = [MkInCompleteChunk a []]
  | (first : second : rest) <- stringOfBrackets,
    second == fromJust (matchingCharacter first) =
    MkChunk first [] : parseChunks rest
  | (first : second : rest) <- stringOfBrackets,
    second /= fromJust (matchingCharacter first),
    Nothing <- contentOfFirstBracket (second : rest) =
    [MkInCompleteChunk first (parseChunks (second : rest))]
  | (first : _ : _) <- stringOfBrackets, -- TODO in dire, dire need of a refactoring
    Just bracketContent <- contentOfFirstBracket stringOfBrackets,
    Just _ <- matchingCharacter first,
    Just partAfterClosingBracket <- stripPrefix bracketContent stringOfBrackets =
    [MkChunk first (parseChunks partAfterClosingBracket)]
  | (first : _ : _) <- stringOfBrackets,
    Just bracketContent <- contentOfFirstBracket stringOfBrackets,
    Just _ <- matchingCharacter first,
    Nothing <- stripPrefix bracketContent stringOfBrackets =
    [MkChunk first (parseChunks bracketContent)]
  | otherwise = error $ "unhandled case during parsing: " ++ stringOfBrackets -- TODO convert to a total function

contentOfFirstBracket :: String -> Maybe String
contentOfFirstBracket input
  | "" <- input = Nothing
  | [_] <- input = Nothing
  | (first : rest) <- input = Just $ contentOfFirstBracket' first 0 rest -- react to the second character having a matching character

contentOfFirstBracket' :: Char -> Int -> String -> String
contentOfFirstBracket' _ _ [] = ""
contentOfFirstBracket' openingBracket bracketBalance (first : rest)
  | bracketBalance > 0 = upUntilNthCharacter bracketBalance closingBracket (first : rest)
  | Nothing <- matchingCharacter openingBracket = first : contentOfFirstBracket' openingBracket bracketBalance rest
  | first == openingBracket = first : contentOfFirstBracket' openingBracket (bracketBalance + 1) rest
  | otherwise = takeWhile (/= closingBracket) (first : rest)
  where
    closingBracket = fromJust $ matchingCharacter openingBracket

upUntilNthCharacter :: Int -> Char -> String -> String
upUntilNthCharacter count character characters
  | count == 0 = characters
  | count == 1 = upUntilNextCharacter
  | otherwise = upUntilNextCharacter ++ upUntilNthCharacter (count - 1) character (tail (dropWhile (/= character) characters))
  where
    upUntilNextCharacter = if character `elem` characters then a ++ [head b] else a
    (a, b) = span (/= character) characters

-- cb aea
-- TODO type synonyms

-- first two don't match -> need to recurse immediately
-- possible terminations of recursion: (should include matched and unmatched cases)
--     rest is empty
-- tools:
-- function to determine if there is a closing bracket before another opening bracket:

matchingCharacter :: Char -> Maybe Char
matchingCharacter '(' = Just ')'
matchingCharacter '[' = Just ']'
matchingCharacter '{' = Just '}'
matchingCharacter '<' = Just '>'
matchingCharacter _ = Nothing

solutionDay10Part2 :: PuzzleInput -> Int
solutionDay10Part2 = const 0

-- TODO avoid unsafe functions entirely

-- from a first and a rest, construct a chunk
-- chunks can be