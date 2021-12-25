module Day08Spec (spec) where

import Day08
  ( solutionDay8Part1,
  -- solutionDay8Part2,
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
      it "should result in xxx" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-08-sample"
        solutionDay8Part1 sampleData `shouldBe` 0

-- context "with actual data for part 1" $
--   it "should result in 341558" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-07"
--     solutionDay7Part1 actualData `shouldBe` 341558
-- context "with sample data for part 2" $
--   it "should result in 168" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-07-sample"
--     solutionDay7Part2 sampleData `shouldBe` 168
-- context "with actual data for part 2" $
--   it "should result in 93214037" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-07"
--     solutionDay7Part2 actualData `shouldBe` 93214037