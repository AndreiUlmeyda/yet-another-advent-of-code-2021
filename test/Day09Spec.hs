module Day09Spec (spec) where

import Day09
  ( solutionDay9Part1,
    solutionDay9Part2,
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
  )

-- runtime: 70ms - 90ms
spec :: Spec
spec = do
  describe "assessing danger through smoke" $ do
    context "with sample data for part 1" $
      it "should result in 15" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-09-sample"
        solutionDay9Part1 sampleData `shouldBe` 15
    context "with actual data for part 1" $
      it "should result in 518" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-09"
        solutionDay9Part1 actualData `shouldBe` 518
    context "with sample data for part 2" $
      it "should result in 1134" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-09-sample"
        solutionDay9Part2 sampleData
          `shouldBe` 1134
    context "with actual data for part 2" $
      it "should result in 949905" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-09"
        solutionDay9Part2 actualData `shouldBe` 949905