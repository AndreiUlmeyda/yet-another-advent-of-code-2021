module Day11Spec (spec) where

import Data.Map (fromList)
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
  -- describe "" $ do
  --   context "with sample data for part 1" $
  --     it "should result in " $ do
  --       sampleData <- lines <$> readFile "puzzle-inputs/day-11-sample"
  --       solutionDay11Part1 sampleData `shouldBe` []
  --   context "with actual data for part 1" $
  --     it "should result in " $ do
  --       actualData <- lines <$> readFile "puzzle-inputs/day-11"
  --       solutionDay11Part1 actualData `shouldBe` []

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
        let singleOctopusAboveThreshold = fromList [((1, 1), MkOctopus 9 0 False)]
        let octopusMarkedAsHavingFlashed = fromList [((1, 1), MkOctopus 9 0 True)]
        resolveFlashes singleOctopusAboveThreshold `shouldBe` octopusMarkedAsHavingFlashed
  context "a flashing octopus next to another octopus" $
    it "should increment the energy of its neighbor" $ do
      let twoOctopuses = fromList [((1, 1), MkOctopus 9 0 False), ((1, 2), MkOctopus 0 0 False)]
      let theSecondOctopusWithIncrementedEnergy = fromList [((1, 1), MkOctopus 9 0 True), ((1, 2), MkOctopus 1 0 False)]
      resolveFlashes twoOctopuses `shouldBe` theSecondOctopusWithIncrementedEnergy