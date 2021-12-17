module Day01Spec (spec) where

import Day01
  ( solutionDay1Part1,
    solutionDay1Part2,
  )
import Test.Hspec (Spec, context, describe, hspec, it, shouldBe)

spec :: Spec
spec = do
  describe "sample data solutions" $ do
    context "part one" $
      it "should be 7" $ do
        solutionDay1Part1 dayOneSampleData `shouldBe` 7 -- full data result 1665
    context "part two" $
      it "part two should be 5" $ do
        solutionDay1Part2 dayOneSampleData `shouldBe` 5 -- full data result 1702

dayOneSampleData :: [String]
dayOneSampleData = ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]