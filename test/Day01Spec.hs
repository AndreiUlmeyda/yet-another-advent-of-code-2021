module Day01Spec (spec) where

import Day01
  ( solutionDay1Part1,
    solutionDay1Part2,
  )
import Test.Hspec (Spec, context, describe, it, shouldBe)

-- runtime: 10ms - 20ms
spec :: Spec
spec = do
  describe "interpreting sonar data" $ do
    context "with sample data for part 1" $
      it "should be 7" $ do
        solutionDay1Part1 dayOneSampleData `shouldBe` 7
    context "with actual data for part 1" $
      it "should be 1665" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-01"
        solutionDay1Part1 actualData `shouldBe` 1665
    context "with sample data for part 2" $
      it "should be 5" $ do
        solutionDay1Part2 dayOneSampleData `shouldBe` 5
    context "with actual data for part 2" $
      it "should be 1702" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-01"
        solutionDay1Part2 actualData `shouldBe` 1702

dayOneSampleData :: [String]
dayOneSampleData = ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]