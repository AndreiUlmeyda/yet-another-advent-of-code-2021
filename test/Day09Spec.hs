module Day09Spec (spec) where

import Data.Array (Array, array, listArray)
import Data.Array.Unboxed (UArray)
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

spec :: Spec
spec = do
  describe "" $ do
    context "with sample data for part 1" $
      it "should result in xxx" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-09-sample"
        solutionDay9Part1 sampleData `shouldBe` [] --listArray (1, 1) [1]

-- context "with actual data for part 1" $
--   it "should result in 355" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-08"
--     solutionDay8Part1 actualData `shouldBe` 355
-- context "with sample data for part 2" $
--   it "should result in 61229" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-08-sample"
--     solutionDay8Part2 sampleData `shouldBe` 61229
-- context "with different sample data for part 2" $
--   it "should result in 9781" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-08-smaller-sample"
--     solutionDay8Part2 sampleData `shouldBe` 9781
-- context "with actual data for part 2" $
--   it "should result in 983030" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-08"
--     solutionDay8Part2 actualData `shouldBe` 983030