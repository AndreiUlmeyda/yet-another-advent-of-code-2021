module Day03
  ( solutionDay3Part1,
    solutionDay3Part2,
    addElementwise,
    filterDiagnosticNumbers,
  )
where

import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Tuple.Extra (both)

solutionDay3Part1 :: [[Char]] -> Int
solutionDay3Part1 = uncurry (*) . both toNumber . pairWithNegation . mostCommonDigits . toDiagnosticNumbers

mostCommonDigits = biggerThanHalfTheInputLength . sumUpElementwise

toDiagnosticNumbers :: [[Char]] -> [[Int]]
toDiagnosticNumbers = map (map digitToInt)

sumUpElementwise :: [[Int]] -> ([Int], Int)
sumUpElementwise a = (foldr addElementwise (repeat 0) a, length a)

addElementwise :: [Int] -> [Int] -> [Int]
addElementwise = zipWith (+)

biggerThanHalfTheInputLength :: ([Int], Int) -> [Bool]
biggerThanHalfTheInputLength (number, originalInputLength) = map ((>= halfTheInputLength) . fromIntegral) number
  where
    halfTheInputLength = fromIntegral originalInputLength / 2

pairWithNegation :: [Bool] -> ([Bool], [Bool])
pairWithNegation input = (input, map not input)

toNumber :: [Bool] -> Int
toNumber = sum . zipWith (*) powersOfTwo . toZerosAndOnes . reverse
  where
    powersOfTwo = iterate (* 2) 1

toZerosAndOnes :: [Bool] -> [Int]
toZerosAndOnes = map (\boolean -> if boolean then 1 else 0)

solutionDay3Part2 = transpose . toDiagnosticNumbers

filterDiagnosticNumbers :: [[Int]] -> [Int]
filterDiagnosticNumbers = filterDiagnosticNumbers' 0 mostCommonDigits
  where
    mostCommonDigits = []

filterDiagnosticNumbers' _ _ _ = []

-- filter numbers by digit at index
-- -> numbers, digit, index

-- from one iteration to the next
--    numbers -> parameter
--    digit -> computed once, declare in outer scope, pass to abc'
--    index -> increases by 1 (parameter?)