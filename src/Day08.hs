module Day08
  ( solutionDay8Part1,
    solutionDay8Part2,
  )
where

import Control.Lens (element, set)
import Data.List (elemIndex, find, findIndex, intersect, sort, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Day04 (PuzzleInput)
import GHC.Conc (Signal)
import Util (toNumberOfBase)

type SevenSegmentDigit = String

-- ######### Part One #########
solutionDay8Part1 :: PuzzleInput -> Int
solutionDay8Part1 = sum . map countUniqueLengths . prepareInput

countUniqueLengths :: [SevenSegmentDigit] -> Int
countUniqueLengths = length . filter isOfUniqueLength

isOfUniqueLength :: Foldable t => t a -> Bool
isOfUniqueLength a = length a `elem` [2, 3, 4, 7]

prepareInput :: PuzzleInput -> [[SevenSegmentDigit]]
prepareInput = map (words . (!! 1) . splitOn " | ")

-- ######### Part Two #########
solutionDay8Part2 :: PuzzleInput -> Int
solutionDay8Part2 = sum . map (toNumberOfBase 10 . applyMapping . pairWithMappings) . prepareInput2

applyMapping :: ([SevenSegmentDigit], [String]) -> [Int]
applyMapping (mapping, output) = map (\string -> debug (elemIndex string mapping) mapping output) output

debug :: Maybe Int -> [SevenSegmentDigit] -> [String] -> Int
debug result mapping output
  | Nothing <- result = error $ show mapping ++ "\n" ++ show output
  | Just a <- result = a

pairWithMappings :: [[String]] -> ([SevenSegmentDigit], [String])
pairWithMappings signalPatternsAndOutput = (inferMappingFrom signalPatterns, output)
  where
    signalPatterns = head signalPatternsAndOutput
    output = signalPatternsAndOutput !! 1

inferMappingFrom :: [SevenSegmentDigit] -> [SevenSegmentDigit]
inferMappingFrom signals =
  ( infer 9 lastMissingMapping
      . infer 0 (segmentsInCommonWithDigitAmongSignalsOfLength 4 5 6)
      . inferFive
      . infer 6 (segmentsInCommonWithDigitAmongSignalsOfLength 1 1 6)
      . infer 3 (segmentsInCommonWithDigitAmongSignalsOfLength 2 1 5)
      . infer 2 (segmentsInCommonWithDigitAmongSignalsOfLength 2 4 5)
      . infer 8 (isOfUniqueLength 7)
      . infer 7 (isOfUniqueLength 3)
      . infer 4 (isOfUniqueLength 4)
      . infer 1 (isOfUniqueLength 2)
  )
    emptyMapping
  where
    infer mappingEntry inferenceStrategy previousMapping = set (element mappingEntry) (inferenceStrategy previousMapping) previousMapping
    isOfUniqueLength targetLength _ = fromJust (find (hasLength targetLength) signals)
    segmentsInCommonWithDigitAmongSignalsOfLength segmentNumber digit signalLength mapping = fromJust (find (\signal -> segmentNumber == length (map (intersect signal) mapping !! digit)) (signalsOfLength signalLength))
    signalsOfLength targetLength = filter (hasLength targetLength) signals
    lastMissingMapping mapping = head (signals \\ mapping)
    inferFive mapping = (set (element 5) $ fromJust $ find (certainDigitsInCommonWith1And4 mapping) $ signalsOfLength 5) mapping
    certainDigitsInCommonWith1And4 mapping relevantSignals = 3 == length (map (intersect relevantSignals) mapping !! 4) && 1 == length (map (intersect relevantSignals) mapping !! 1)

-- TODO generalize further to include inference of '5', clean up the where clause, try to clarify further with types / type synonyms

emptyMapping :: [String]
emptyMapping = replicate 10 ""

hasLength :: Foldable t => Int -> t a -> Bool
hasLength n list = length list == n

prepareInput2 :: PuzzleInput -> [[[String]]]
prepareInput2 = map (map (map sort . words) . splitOn " | ")

-- inferMapping :: [[String]] -> (Int, [Int])

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
