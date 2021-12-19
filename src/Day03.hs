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

solutionDay3Part1 :: [[Char]] -> Int
solutionDay3Part1 = uncurry (*) . both toNumber . pairWithNegation . mostCommonDigitsOf . toDiagnosticNumbers

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

flipZerosAndOnes :: (Eq a, Num a, Num p) => a -> p
flipZerosAndOnes digit = if digit == 0 then 1 else 0

toNumber :: [Int] -> Int
toNumber = sum . zipWith (*) powersOfTwo . reverse
  where
    powersOfTwo = iterate (* 2) 1

toZerosAndOnes :: [Bool] -> [Int]
toZerosAndOnes = map (\boolean -> if boolean then 1 else 0)

solutionDay3Part2 :: [[Char]] -> Int
solutionDay3Part2 = uncurry (*) . both toNumber . pairTheTwoRatings . toDiagnosticNumbers

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