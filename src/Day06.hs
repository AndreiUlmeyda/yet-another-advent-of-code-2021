module Day06
  ( solutionDay6Part1,
    solutionDay6Part2,
    FishPopulation (MkFishPopulation),
  )
where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import Day04 (PuzzleInput)

data FishPopulation = MkFishPopulation
  { age0 :: Int,
    age1 :: Int,
    age2 :: Int,
    age3 :: Int,
    age4 :: Int,
    age5 :: Int,
    age6 :: Int,
    age7 :: Int,
    age8 :: Int
  }
  deriving (Show, Eq)

solutionDay6Part1 :: PuzzleInput -> Int
solutionDay6Part1 = populationSizeAfterDays 80

populationSizeAfterDays :: Int -> [[Char]] -> Int
populationSizeAfterDays days = length . (!! days) . iterate (concatMap ageAndMultiply) . prepareInput

prepareInput :: PuzzleInput -> [Int]
prepareInput = map read . splitOn "," . head

ageAndMultiply :: Int -> [Int]
ageAndMultiply age
  | age == 0 = [6, 8]
  | otherwise = [age -1]

solutionDay6Part2 :: PuzzleInput -> Int
solutionDay6Part2 = sumPopulation . (!! 256) . iterate ageAndMultiplyImproved . toFishPopulation . map (\a -> (head a, length a)) . group . sort . prepareInput

sumPopulation :: FishPopulation -> Int
sumPopulation population = age0 population + age1 population + age2 population + age3 population + age4 population + age5 population + age6 population + age7 population + age8 population

ageAndMultiplyImproved :: FishPopulation -> FishPopulation
ageAndMultiplyImproved population =
  MkFishPopulation
    { age0 = age1 population,
      age1 = age2 population,
      age2 = age3 population,
      age3 = age4 population,
      age4 = age5 population,
      age5 = age6 population,
      age6 = age7 population + age0 population,
      age7 = age8 population,
      age8 = age0 population
    }

toFishPopulation :: [(Int, Int)] -> FishPopulation
toFishPopulation = foldr derp (MkFishPopulation 0 0 0 0 0 0 0 0 0)

derp :: (Int, Int) -> FishPopulation -> FishPopulation
derp ageAndCount population
  | fst ageAndCount == 0 = population {age0 = snd ageAndCount}
  | fst ageAndCount == 1 = population {age1 = snd ageAndCount}
  | fst ageAndCount == 2 = population {age2 = snd ageAndCount}
  | fst ageAndCount == 3 = population {age3 = snd ageAndCount}
  | fst ageAndCount == 4 = population {age4 = snd ageAndCount}
  | fst ageAndCount == 5 = population {age5 = snd ageAndCount}
  | fst ageAndCount == 6 = population {age6 = snd ageAndCount}
  | fst ageAndCount == 7 = population {age7 = snd ageAndCount}
  | otherwise = population {age8 = fst ageAndCount}