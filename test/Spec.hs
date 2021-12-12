import Lib
  ( SubDirection (Down, Forward, Up),
    SubMovement (MkSubMovement),
    SubMovementPlus (MkSubMovementPlus),
    computeAim,
    solutionDay1Part1,
    solutionDay1Part2,
    solutionDay2Part1,
    solutionDay2Part2,
    toSubMovementPlus,
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
      -- it "the solution to part two with sample data should be x" $ do
      --   solutionDay2Part2 dayTwoSampleData `shouldBe` 900 -- full data result ...
  describe "parsing sub movements" $ do
    it "given a forward direction and magnitude 5 should parse them" $ do
      let input = ["forward", "5"]
       in toSubMovementPlus input `shouldBe` MkSubMovementPlus Forward 5 0
    it "given a downward direction and magnitude 1 should parse them" $ do
      let input = ["down", "1"]
       in toSubMovementPlus input `shouldBe` MkSubMovementPlus Down 1 0
    it "given an upward direction and magnitude 7 should parse them" $ do
      let input = ["up", "7"]
       in toSubMovementPlus input `shouldBe` MkSubMovementPlus Up 7 0

-- describe "computing the aim" $ do
--   it "given a forward direction should leave the aim unchanged" $ do
--     let movement = MkSubMovement 5 0
--      in computeAim movement `shouldBe` movement
--   it "given a downward direction should increase the aim by 5" $ do
--     computeAim (MkSubMovement 0 1) `shouldBe` MkSubMovement 0 1
--   it "given an upward direction should decrease the aim by 3" $ do
--     computeAim (MkSubMovement 0 (-5)) `shouldBe` MkSubMovement 0 (-5)

dayOneSampleData :: [String]
dayOneSampleData = ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]

dayTwoSampleData :: [String]
dayTwoSampleData = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]