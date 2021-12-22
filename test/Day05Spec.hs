module Day05Spec (spec) where

import Day05
  ( solutionDay5Part1,
    solutionDay5Part2,
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
  describe "avoiding hydrothermal vents" $ do
    context "with sample data for part 1" $
      it "should result in xxx" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-05-sample"
        solutionDay5Part1 sampleData `shouldBe` [(0, 9), (1, 9), (2, 9), (3, 9), (4, 9), (5, 9), (3, 4), (4, 4), (5, 4), (6, 4), (7, 4), (8, 4), (9, 4), (1, 2), (2, 2), (0, 7), (1, 7), (2, 7), (3, 7), (4, 7), (0, 9), (1, 9), (2, 9), (1, 4), (2, 4), (3, 4)]

-- context "with actual data for part 1" $
--   it "should result in xxx" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-05"
--     solutionDay5Part1 actualData `shouldBe` 0
-- context "with sample data for part 2" $
--   it "should result in xxx" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-05-sample"
--     solutionDay5Part2 sampleData `shouldBe` 0
-- context "with actual data for part 2" $
--   it "should result in xxx" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-05"
--     solutionDay5Part2 actualData `shouldBe` 0