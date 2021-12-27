module Day03
  ( solutionDay3Part1,
    solutionDay3Part2,
    addElementwise,
    filterDiagnosticNumbers,
    toDiagnosticNumbers,
    mostCommonDigitsOf,
    leastCommonDigitsOf,
  )
where

import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Tuple.Extra (both)
import Util (toNumberOfBase)

-- ######### Part One #########
solutionDay3Part1 :: [[Char]] -> Int
solutionDay3Part1 = uncurry (*) . both (toNumberOfBase 2) . pairWithNegation . mostCommonDigitsOf . toDiagnosticNumbers

mostCommonDigitsOf :: [[Int]] -> [Int]
mostCommonDigitsOf = toZerosAndOnes . biggerThanHalfTheInputLength . elementwiseSumAndLenght

leastCommonDigitsOf :: [[Int]] -> [Int]
leastCommonDigitsOf = map flipZerosAndOnes . mostCommonDigitsOf

toDiagnosticNumbers :: [[Char]] -> [[Int]]
toDiagnosticNumbers = map (map digitToInt)

elementwiseSumAndLenght :: [[Int]] -> ([Int], Int)
elementwiseSumAndLenght numbers = (foldr addElementwise (repeat 0) numbers, length numbers)

addElementwise :: [Int] -> [Int] -> [Int]
addElementwise = zipWith (+)

biggerThanHalfTheInputLength :: ([Int], Int) -> [Bool]
biggerThanHalfTheInputLength (number, originalInputLength) = map ((>= halfTheInputLength) . fromIntegral) number
  where
    halfTheInputLength = fromIntegral originalInputLength / 2

pairWithNegation :: [Int] -> ([Int], [Int])
pairWithNegation input = (input, map flipZerosAndOnes input)

flipZerosAndOnes :: (Eq p, Num p) => p -> p
flipZerosAndOnes digit
  | 0 <- digit = 1
  | otherwise = 0

toZerosAndOnes :: [Bool] -> [Int]
toZerosAndOnes = map (\boolean -> if boolean then 1 else 0)

-- ######### Part Two #########
solutionDay3Part2 :: [[Char]] -> Int
solutionDay3Part2 = uncurry (*) . both (toNumberOfBase 2) . pairTheTwoRatings . toDiagnosticNumbers

pairTheTwoRatings :: [[Int]] -> ([Int], [Int])
pairTheTwoRatings numbers = (oxygenGeneratorRating numbers, co2ScrubberRating numbers)

oxygenGeneratorRating :: [[Int]] -> [Int]
oxygenGeneratorRating = filterDiagnosticNumbers mostCommonDigitsOf

co2ScrubberRating :: [[Int]] -> [Int]
co2ScrubberRating = filterDiagnosticNumbers leastCommonDigitsOf

filterDiagnosticNumbers :: ([[Int]] -> [Int]) -> [[Int]] -> [Int]
filterDiagnosticNumbers = filterDiagnosticNumbers' 0

filterDiagnosticNumbers' :: Eq a => Int -> ([[a]] -> [a]) -> [[a]] -> [a]
filterDiagnosticNumbers' index criterion numbers
  | [singleNumber] <- numbers = singleNumber
  | otherwise = filterDiagnosticNumbers' (index + 1) criterion $ filter digitMatchesCriterion numbers
  where
    digitMatchesCriterion number = number !! index == criterion numbers !! index

-- filter numbers by digit at index
-- -> numbers, digit, index

-- from one iteration to the next
--    numbers -> parameter
--    digit -> compute once, declare in outer scope, pass to abc'
--    index -> increases by 1 (parameter?)