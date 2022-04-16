module Day11Spec (spec) where

import Data.Map (fromList, toList)
import Day11
  ( Octopus (MkOctopus),
    increaseOctopusEnergies,
    resolveFlashes,
    solutionDay11Part1,
    solutionDay11Part2,
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "" $ do
    context "with sample data for part 2" $
      it "should result in " $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-11-sample"
        solutionDay11Part2 sampleData `shouldBe` 0
    context "with actual data for part 2" $
      it "should result in " $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-10"
        solutionDay11Part2 actualData `shouldBe` 0

  describe "increasing octopus energy for one step" $ do
    context "a single octopus" $
      it "should just increment its value" $ do
        increaseOctopusEnergies (fromList [((1, 1), MkOctopus 0 0 False)]) `shouldBe` fromList [((1, 1), MkOctopus 1 0 False)]

  describe "resolving flashes" $ do
    context "a single octopus below the threshold" $
      it "should not flash" $ do
        let singleOctopusBelowThreshold = fromList [((1, 1), MkOctopus 3 0 False)]
        resolveFlashes singleOctopusBelowThreshold `shouldBe` singleOctopusBelowThreshold
    context "a single octopus above the threshold" $
      it "should be marked as having flashed" $ do
        let singleOctopusAboveThreshold = fromList [((1, 1), MkOctopus 10 0 False)]
        let octopusMarkedAsHavingFlashed = fromList [((1, 1), MkOctopus 0 1 False)]
        resolveFlashes singleOctopusAboveThreshold `shouldBe` octopusMarkedAsHavingFlashed
    context "a flashing octopus next to another octopus" $
      it "should increment the energy of its neighbor" $ do
        let twoOctopuses = fromList [((1, 1), MkOctopus 10 0 False), ((1, 2), MkOctopus 0 0 False)]
        let theSecondOctopusWithIncrementedEnergy = fromList [((1, 1), MkOctopus 0 1 False), ((1, 2), MkOctopus 1 0 False)]
        resolveFlashes twoOctopuses `shouldBe` theSecondOctopusWithIncrementedEnergy
    context "two flashing octopuses next to another octopus" $
      it "should increment the energy twice" $ do
        let twoFlashingOctopuses = fromList [((1, 1), MkOctopus 0 0 False), ((1, 2), MkOctopus 10 0 False), ((2, 1), MkOctopus 10 0 False)]
        let middleOctopusIncrementedTwice = fromList [((1, 1), MkOctopus 2 0 False), ((1, 2), MkOctopus 0 1 False), ((2, 1), MkOctopus 0 1 False)]
        resolveFlashes twoFlashingOctopuses `shouldBe` middleOctopusIncrementedTwice
    context "..." $
      it "derp" $ do
        let a = fromList [((0, 1), MkOctopus 7 0 False), ((0, 2), MkOctopus 6 0 False), ((0, 3), MkOctopus 10 0 False), ((0, 4), MkOctopus 5 0 False), ((1, 1), MkOctopus 4 0 False), ((1, 2), MkOctopus 9 0 False), ((1, 3), MkOctopus 6 0 False), ((1, 4), MkOctopus 7 0 False)]
        let b = fromList [((0, 1), MkOctopus 8 0 False), ((0, 2), MkOctopus 8 0 False), ((0, 3), MkOctopus 0 1 False), ((0, 4), MkOctopus 6 0 False), ((1, 1), MkOctopus 5 0 False), ((1, 2), MkOctopus 0 1 False), ((1, 3), MkOctopus 8 0 False), ((1, 4), MkOctopus 8 0 False)]
        resolveFlashes a `shouldBe` b

  -- context "" $
  --   it "should " $ do
  --     (show . map snd) (toList ((resolveFlashes . increaseOctopusEnergies) sample)) `shouldBe` (show . map snd) (toList sampleAfterOneStep)

  describe "" $ do
    context "with sample data for part 1" $
      it "should result in 1656" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-11-sample"
        solutionDay11Part1 sampleData `shouldBe` 1656

    context "with actual data for part 1" $
      it "should result in " $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-11"
        solutionDay11Part1 actualData `shouldBe` 0

-- context "a flashing octopus next to another octopus" $
--   it "should increment the energy of its neighbor" $ do
--     let twoOctopuses = fromList [((1, 1), MkOctopus 9 0 False), ((1, 2), MkOctopus 0 0 False)]
--     let theSecondOctopusWithIncrementedEnergy = fromList [((1, 1), MkOctopus 9 0 True), ((1, 2), MkOctopus 1 0 False)]
--     resolveFlashes twoOctopuses `shouldBe` theSecondOctopusWithIncrementedEnergy

-- 111
-- 199

-- sample =
--   fromList
--     [ ((1, 1), MkOctopus 1 0 False),
--       ((1, 2), MkOctopus 1 0 False),
--       ((1, 3), MkOctopus 1 0 False),
--       ((2, 1), MkOctopus 1 0 False),
--       ((2, 2), MkOctopus 9 0 False),
--       ((2, 3), MkOctopus 9 0 False)
--     ]

-- -- 344
-- -- 300

-- sampleAfterOneStep =
--   fromList
--     [ ((1, 1), MkOctopus 3 0 False),
--       ((1, 2), MkOctopus 4 0 False),
--       ((1, 3), MkOctopus 4 0 False),
--       ((2, 1), MkOctopus 3 0 False),
--       ((2, 2), MkOctopus 0 1 False),
--       ((2, 3), MkOctopus 0 1 False)
--     ]

sample =
  fromList
    [ ((1, 1), MkOctopus 1 0 False),
      ((1, 2), MkOctopus 1 0 False),
      ((1, 3), MkOctopus 1 0 False),
      ((1, 4), MkOctopus 1 0 False),
      ((1, 5), MkOctopus 1 0 False),
      ((2, 1), MkOctopus 1 0 False),
      ((2, 2), MkOctopus 9 0 False),
      ((2, 3), MkOctopus 9 0 False),
      ((2, 4), MkOctopus 9 0 False),
      ((2, 5), MkOctopus 1 0 False),
      ((3, 1), MkOctopus 1 0 False),
      ((3, 2), MkOctopus 9 0 False),
      ((3, 3), MkOctopus 1 0 False),
      ((3, 4), MkOctopus 9 0 False),
      ((3, 5), MkOctopus 1 0 False),
      ((4, 1), MkOctopus 1 0 False),
      ((4, 2), MkOctopus 9 0 False),
      ((4, 3), MkOctopus 9 0 False),
      ((4, 4), MkOctopus 9 0 False),
      ((4, 5), MkOctopus 1 0 False),
      ((5, 1), MkOctopus 1 0 False),
      ((5, 2), MkOctopus 1 0 False),
      ((5, 3), MkOctopus 1 0 False),
      ((5, 4), MkOctopus 1 0 False),
      ((5, 5), MkOctopus 1 0 False)
    ]

sampleAfterOneStep =
  fromList
    [ ((1, 1), MkOctopus 3 0 False),
      ((1, 2), MkOctopus 4 0 False),
      ((1, 3), MkOctopus 5 0 False),
      ((1, 4), MkOctopus 4 0 False),
      ((1, 5), MkOctopus 3 0 False),
      ((2, 1), MkOctopus 4 0 False),
      ((2, 2), MkOctopus 0 0 False),
      ((2, 3), MkOctopus 0 0 False),
      ((2, 4), MkOctopus 0 0 False),
      ((2, 5), MkOctopus 4 0 False),
      ((3, 1), MkOctopus 5 0 False),
      ((3, 2), MkOctopus 0 0 False),
      ((3, 3), MkOctopus 0 0 False),
      ((3, 4), MkOctopus 0 0 False),
      ((3, 5), MkOctopus 5 0 False),
      ((4, 1), MkOctopus 4 0 False),
      ((4, 2), MkOctopus 0 0 False),
      ((4, 3), MkOctopus 0 0 False),
      ((4, 4), MkOctopus 0 0 False),
      ((4, 5), MkOctopus 4 0 False),
      ((5, 1), MkOctopus 3 0 False),
      ((5, 2), MkOctopus 4 0 False),
      ((5, 3), MkOctopus 5 0 False),
      ((5, 4), MkOctopus 4 0 False),
      ((5, 5), MkOctopus 3 0 False)
    ]

-- 11111
-- 19991
-- 19191
-- 19991
-- 11111