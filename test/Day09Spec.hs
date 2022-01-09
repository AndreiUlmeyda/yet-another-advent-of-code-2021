module Day09Spec (spec) where

import Data.Array.Unboxed (UArray, array, bounds, inRange, indices, (!))
import Data.Map (fromList)
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

-- context "with different sample data for part 2" $
--   it "should result in 9781" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-08-smaller-sample"
--     solutionDay8Part2 sampleData `shouldBe` 9781

-- fromList
-- [ ((0, 0), (0, 1)),
--   ((0, 1), (0, 1)),
--   ((0, 2), (-1, -1)),
--   ((0, 3), (-1, -1)),
--   ((0, 4), (-1, -1)),
--   ((0, 5), (0, 9)),
--   ((0, 6), (0, 9)),
--   ((0, 7), (0, 9)),
--   ((0, 8), (0, 9)),
--   ((0, 9), (0, 9)),
--   ((1, 0), (0, 1)),
--   ((1, 1), (-1, -1)),
--   ((1, 2), (2, 2)),
--   ((1, 3), (-1, -1)),
--   ((1, 4), (-1, -1)),
--   ((1, 5), (0, 9)),
--   ((1, 6), (0, 9)),
--   ((1, 7), (0, 9)),
--   ((1, 8), (0, 9)),
--   ((1, 9), (0, 9))
-- ] -- (array ((0, 0), (0, 1)) [((0, 0), 0)] :: UArray (Int, Int) Int) --(1, [1]) --