module Day11Spec (spec) where

import Day11
  ( solutionDay11Part1,
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
    context "with sample data for part 1" $
      it "should result in " $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-11-sample"
        solutionDay11Part1 sampleData `shouldBe` 0
    context "with actual data for part 1" $
      it "should result in " $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-11"
        solutionDay11Part1 actualData `shouldBe` 0

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