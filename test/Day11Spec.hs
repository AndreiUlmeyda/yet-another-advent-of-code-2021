module Day11Spec (spec) where

import Data.Array (listArray)
import Day11
  ( Octopus (MkOctopus),
    increaseOctopusEnergies,
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
        solutionDay11Part2 sampleData
          `shouldBe` 0
    context "with actual data for part 2" $
      it "should result in " $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-10"
        solutionDay11Part2 actualData `shouldBe` 0

  describe "counting octopus flashes" $ do
    context "a single octopus" $
      it "should just increment its value" $ do
        increaseOctopusEnergies (listArray (1, 1) [MkOctopus 0 0]) `shouldBe` listArray (1, 1) [MkOctopus 1 0]

-- context "a single octopus with value 9" $
--   it "should reset its value to zero and increment the flash counter" $ do
--     increaseOctopusEnergies (listArray (1, 1) [MkOctopus 9 0]) `shouldBe` listArray (1, 1) [MkOctopus 0 1]