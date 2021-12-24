module Day07Spec (spec) where

import Day07
  ( solutionDay7Part1,
    solutionDay7Part2,
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
  describe "aligning battle crabs" $ do
    context "with sample data for part 1" $
      it "should result in 37" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-07-sample"
        solutionDay7Part1 sampleData `shouldBe` 37
    context "with actual data for part 1" $
      it "should result in 341558" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-07"
        solutionDay7Part1 actualData `shouldBe` 341558
    context "with sample data for part 2" $
      it "should result in 168" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-07-sample"
        solutionDay7Part2 sampleData `shouldBe` 168
    context "with actual data for part 2" $
      it "should result in xxx" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-07"
        solutionDay7Part2 actualData `shouldBe` 341558