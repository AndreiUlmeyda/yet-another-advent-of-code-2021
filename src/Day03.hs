module Day03
  ( solutionDay3Part1,
    solutionDay3Part2,
    addElementwise,
  )
where

import Data.Char (digitToInt)
import Data.Tuple.Extra (both)

solutionDay3Part1 :: [[Char]] -> Int
solutionDay3Part1 input = (uncurry (*) . both toNumber . pairWithNegation . biggerThanHalfTheInputLength input . sumUpElementwise . toDiagnosticNumbers) input

toNumber :: [Bool] -> Int
toNumber = sum . zipWith (*) powersOfTwo . toZerosAndOnes . reverse
  where
    powersOfTwo = iterate (* 2) 1

toZerosAndOnes :: [Bool] -> [Int]
toZerosAndOnes = map (\boolean -> if boolean then 1 else 0)

pairWithNegation :: [Bool] -> ([Bool], [Bool])
pairWithNegation input = (input, map not input)

biggerThanHalfTheInputLength :: [String] -> [Int] -> [Bool]
biggerThanHalfTheInputLength inputStrings = map ((>= halfTheInputLength) . fromIntegral)
  where
    halfTheInputLength = fromIntegral (length inputStrings) / 2 :: Float

sumUpElementwise :: [[Int]] -> [Int]
sumUpElementwise = foldr addElementwise (repeat 0)

toDiagnosticNumbers :: [[Char]] -> [[Int]]
toDiagnosticNumbers = map (map digitToInt)

addElementwise :: [Int] -> [Int] -> [Int]
addElementwise = zipWith (+)

-- solutionDay3Part1 = foldr justTheSecondOne ""

justTheSecondOne :: p1 -> p2 -> p1
justTheSecondOne a b = a

solutionDay3Part2 :: [String] -> Int
solutionDay3Part2 = const 0