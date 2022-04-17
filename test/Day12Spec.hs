module Day12Spec (spec) where

import Day12
  ( solutionDay12Part1,
    solutionDay12Part2,
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
      it "should result in x" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-12-sample"
        solutionDay12Part1 sampleData `shouldBe` 1
    context "with actual data for part 1" $
      it "should result in x" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-12"
        solutionDay12Part1 actualData `shouldBe` 1
    context "with sample data for part 2" $
      it "should result in x" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-12-sample"
        solutionDay12Part2 sampleData `shouldBe` 2
    context "with actual data for part 2" $
      it "should result in x" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-12"
        solutionDay12Part2 actualData `shouldBe` 2