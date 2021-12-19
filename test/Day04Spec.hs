module Day04Spec (spec) where

import Day04
  ( solutionDay4Part1,
    solutionDay4Part2,
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
  describe "playing bingo" $ do
    context "with sample data for part 1" $
      it "should be 4512" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-04-sample"
        solutionDay4Part1 sampleData `shouldBe` 4512
