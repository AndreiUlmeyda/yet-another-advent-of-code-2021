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
firstCorruptCharacter = firstCorruptCharacter' []

-- firstCorruptCharacter [] = Nothing
-- firstCorruptCharacter [singleBracket] = if isClosingCharacter singleBracket then Just singleBracket else Nothing
-- firstCorruptCharacter (first : second : rest)
--   | isOpeningCharacter second = firstCorruptCharacter (second : rest)
--   | otherwise = if first `isClosedBy` second then firstCorruptCharacter rest else Just second

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

--   | (not . null) openedBrackets,
--     last openedBrackets `isClosedBy` first =
--     firstCorruptCharacter' (init openedBrackets) rest
--   | first `isClosedBy` second = firstCorruptCharacter' openedBrackets rest
--   | otherwise = Just second
--   where
--     second = head rest

-- firstCorruptCharacter' :: String -> String -> Maybe Char
-- firstCorruptCharacter' _ [] = Nothing
-- firstCorruptCharacter' openedBrackets [singleBracket] =
--   if isClosingCharacter singleBracket && singleBracket `notElem` map matchingCharacter' openedBrackets
--     then Just singleBracket
--     else Nothing
-- firstCorruptCharacter' openedBrackets (first : second : rest)
--   | (not . null) openedBrackets,
--     last openedBrackets `isClosedBy` first =
--     firstCorruptCharacter' (init openedBrackets) (second : rest)
--   | isOpeningCharacter second = firstCorruptCharacter' (openedBrackets ++ [first]) (second : rest)
--   | first `isClosedBy` second = firstCorruptCharacter' openedBrackets rest
--   | otherwise = Just second

isOpeningCharacter :: Char -> Bool
isOpeningCharacter = not . isClosingCharacter

isClosingCharacter :: Char -> Bool
isClosingCharacter character = character `elem` ")}]>"

-- fst  snd
-- o    o -> yes
-- o    c -> only if they match
-- c    o -> only if it closes something / yes
-- c    c -> only if it closes something

-- closes :: Char -> Char -> Bool
-- closes second first
--   | Nothing <- matchingCharacter first = False
--   | Just match <- matchingCharacter first = match == second

-- test case: first character is a closing one
-- the last recursion needs to recurse solely on the part until the next closing
-- bracket of first

-- {(... -> fst {, snd (, rest ... => ask about whether fCC rest returns anything
-- => returns nothing ->
-- => returns something ->

-- rest OR second : rest ???? =>

-- (<)>

-- {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
-- [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
-- [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
-- [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
-- <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

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

matchingCharacter' :: Char -> Char
matchingCharacter' '(' = ')'
matchingCharacter' '[' = ']'
matchingCharacter' '{' = '}'
matchingCharacter' '<' = '>'
matchingCharacter' _ = error "fuck me"

isClosedBy :: Char -> Char -> Bool
isClosedBy '(' ')' = True
isClosedBy '[' ']' = True
isClosedBy '{' '}' = True
isClosedBy '<' '>' = True
isClosedBy _ _ = False

solutionDay10Part2 :: PuzzleInput -> Int
solutionDay10Part2 = const 0

-- TODO avoid unsafe functions entirely
