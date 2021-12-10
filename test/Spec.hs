import Lib
  ( SubmarineMovement (MkSubmarineMovement),
    computeAim,
    solutionDay1Part1,
    solutionDay1Part2,
    solutionDay2Part1,
    solutionDay2Part2,
  )
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "day 1" $ do
    it "the solution to part one with sample data should be 7" $ do
      solutionDay1Part1 dayOneSampleData `shouldBe` 7 -- full data result 1665
    it "the solution to part two with sample data should be 5" $ do
      solutionDay1Part2 dayOneSampleData `shouldBe` 5 -- full data result 1702
  describe "day 2" $ do
    it "the solution to part one with sample data should be 150" $ do
      solutionDay2Part1 dayTwoSampleData `shouldBe` 150 -- full data result 1727835
    it "the solution to part two with sample data should be x" $ do
      solutionDay2Part2 dayTwoSampleData `shouldBe` 900 -- full data result ...
  describe "computing the aim" $ do
    it "given a forward direction should leave the aim unchanged" $ do
      let movement = MkSubmarineMovement 5 0 0
       in computeAim movement `shouldBe` movement
    it "given a downward direction should increase the aim by 5" $ do
      computeAim (MkSubmarineMovement 0 1 0) `shouldBe` MkSubmarineMovement 0 1 5
    it "given an upward direction should decrease the aim by 3" $ do
      computeAim (MkSubmarineMovement 0 (-5) 6) `shouldBe` MkSubmarineMovement 0 (-5) 3

dayOneSampleData :: [String]
dayOneSampleData = ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]

dayTwoSampleData :: [String]
dayTwoSampleData = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]