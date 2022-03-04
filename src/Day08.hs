module Day08
  ( solutionDay8Part1,
    solutionDay8Part2,
  )
where

import Control.Lens (element, set)
import Data.List (elemIndex, find, intersect, sort, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Day04 (PuzzleInput)
import Util (toNumberOfBase)
import Prelude

type SevenSegmentDigit = String

-- ######### Part One #########
solutionDay8Part1 :: PuzzleInput -> Int
solutionDay8Part1 = sum . map countUniqueLengths . prepareInput

countUniqueLengths :: [SevenSegmentDigit] -> Int
countUniqueLengths = length . filter (isOfEitherLength [2, 3, 4, 7])

isOfEitherLength :: [Int] -> [a] -> Bool
isOfEitherLength validLengths a = length a `elem` validLengths

prepareInput :: PuzzleInput -> [[SevenSegmentDigit]]
prepareInput = map (words . (!! 1) . splitOn " | ")

-- ######### Part Two #########
solutionDay8Part2 :: PuzzleInput -> Int
solutionDay8Part2 = sum . map (toNumberOfBase 10 . applyMapping . pairWithMappings) . prepareInput2

applyMapping :: ([SevenSegmentDigit], [String]) -> [Int]
applyMapping (mapping, output) = map (\string -> fromJust (elemIndex string mapping)) output

pairWithMappings :: [[String]] -> ([SevenSegmentDigit], [String])
pairWithMappings signalPatternsAndOutput = (inferMappingFrom signalPatterns, output)
  where
    signalPatterns = head signalPatternsAndOutput
    output = signalPatternsAndOutput !! 1

inferMappingFrom :: [SevenSegmentDigit] -> [SevenSegmentDigit]
inferMappingFrom signals =
  ( infer 9 (lastMissingMapping signals)
      . infer 0 (segmentsInCommonWithDigitAmongSignalsOfLength signals 4 5 6)
      . inferFive signals
      . infer 6 (segmentsInCommonWithDigitAmongSignalsOfLength signals 1 1 6)
      . infer 3 (segmentsInCommonWithDigitAmongSignalsOfLength signals 2 1 5)
      . infer 2 (segmentsInCommonWithDigitAmongSignalsOfLength signals 2 4 5)
      . infer 8 (isOfUniqueLength signals 7)
      . infer 7 (isOfUniqueLength signals 3)
      . infer 4 (isOfUniqueLength signals 4)
      . infer 1 (isOfUniqueLength signals 2)
  )
    emptyMapping

infer :: Int -> ([SevenSegmentDigit] -> SevenSegmentDigit) -> [SevenSegmentDigit] -> [SevenSegmentDigit]
infer mappingEntry inferenceStrategy previousMapping = set (element mappingEntry) (inferenceStrategy previousMapping) previousMapping

segmentsInCommonWithDigitAmongSignalsOfLength :: [SevenSegmentDigit] -> Int -> Int -> Int -> [SevenSegmentDigit] -> SevenSegmentDigit
segmentsInCommonWithDigitAmongSignalsOfLength signals segmentNumber digit signalLength mapping = fromJust (find (\signal -> segmentNumber == length (map (intersect signal) mapping !! digit)) (signalsOfLength signalLength signals))

inferFive :: [SevenSegmentDigit] -> [SevenSegmentDigit] -> [SevenSegmentDigit]
inferFive signals mapping = (set (element 5) $ fromJust $ find (certainDigitsInCommonWith1And4 mapping) $ signalsOfLength 5 signals) mapping

certainDigitsInCommonWith1And4 :: Eq a => [[a]] -> [a] -> Bool
certainDigitsInCommonWith1And4 mapping relevantSignals = 3 == length (map (intersect relevantSignals) mapping !! 4) && 1 == length (map (intersect relevantSignals) mapping !! 1)

isOfUniqueLength :: [SevenSegmentDigit] -> Int -> [SevenSegmentDigit] -> SevenSegmentDigit
isOfUniqueLength signals targetLength = const $ fromJust (find (hasLength targetLength) signals)

lastMissingMapping :: [SevenSegmentDigit] -> [SevenSegmentDigit] -> SevenSegmentDigit
lastMissingMapping signals mapping = head (signals \\ mapping)

signalsOfLength :: Int -> [SevenSegmentDigit] -> [SevenSegmentDigit]
signalsOfLength targetLength = filter (hasLength targetLength)

-- TODO generalize further to include inference of '5', try to clarify further with types / type synonyms

emptyMapping :: [String]
emptyMapping = replicate 10 ""

hasLength :: Foldable t => Int -> t a -> Bool
hasLength n list = length list == n

prepareInput2 :: PuzzleInput -> [[[String]]]
prepareInput2 = map (map (map sort . words) . splitOn " | ")

--     be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
--  => be abcdefg bcdefg acdefg bceg cdefg abdefg bcdef abcdf bde | abcdefg bcdef bcdefg bceg
--  => 1  8                     4                             7
--
-- step one: infer uniques 1 4 7 8
-- 1: [x, y] 4: [k,l,m,n] 7: [p,q,r] 8: [a,b,c,d,e,f,g]
-- remaining counts 2 -> 5
--                  3 -> 5
--                  5 -> 5
--                  6 -> 6
--                  9 -> 6
--                  0 -> 6
-- step two: infer 6 segment digits
--  observation: can be decided knowing whether both or only one of the segments for '1' are present, both -> '9' one -> '6'
--  UPDATE: I missed that 0 has 6 segments as well
-- step three: infer 5 segment digits
--  observation: analogously to step 2, if both segments for '1' are present its '3', otherwise it can be decided by how many overlaps
--               there are with '4' -> 2 segments for '2', 3 segments for 5
--  UPDATE: one correct rule set is:
--    - its a 6 if it has 6 segments and it has 1 segment in common with 1
--    - its a 0 if it has 6 segments and it has 4 segments in common with 5
--    - its a 9 if it has 6 segments and its the only number noch previously inferred
