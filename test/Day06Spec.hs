module Day06Spec (spec) where

import Day06
  ( solutionDay6Part1,
    solutionDay6Part2,
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
  describe "modeling lanternfish populations" $ do
    context "with sample data for part 1" $
      it "should result in 5934" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-06-sample"
        solutionDay6Part1 sampleData `shouldBe` 5934
    context "with actual data for part 1" $
      it "should result in 393019" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-06"
        solutionDay6Part1 actualData `shouldBe` 393019
    context "with sample data for part 2" $
      it "should result in 26984457539" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-06-sample"
        solutionDay6Part2 sampleData `shouldBe` 26984457539
    context "with actual data for part 1" $
      it "should result in 1757714216975" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-06"
        solutionDay6Part2 actualData `shouldBe` 1757714216975