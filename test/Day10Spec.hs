module Day10Spec (spec) where

import Day10
  ( solutionDay10Part1,
  -- solutionDay10Part2,
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
        sampleData <- lines <$> readFile "puzzle-inputs/day-10-sample"
        solutionDay10Part1 sampleData `shouldBe` 0

-- context "with actual data for part 1" $
--   it "should result in 518" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-09"
--     solutionDay9Part1 actualData `shouldBe` 518
-- context "with sample data for part 2" $
--   it "should result in 1134" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-09-sample"
--     solutionDay9Part2 sampleData
--       `shouldBe` 1134
-- context "with actual data for part 2" $
--   it "should result in 949905" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-09"
--     solutionDay9Part2 actualData `shouldBe` 949905