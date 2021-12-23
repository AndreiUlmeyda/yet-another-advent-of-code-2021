module Day06Spec (spec) where

import Data.List (sort)
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
  describe "" $ do
    context "with sample data for part 1" $
      it "should result in 5934" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-06-sample"
        solutionDay6Part1 sampleData `shouldBe` 5934
    context "with actual data for part 1" $
      it "should result in 393019" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-06"
        solutionDay6Part1 actualData `shouldBe` 393019