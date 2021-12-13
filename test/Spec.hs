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
      toSubMovementPlus ["forward", "5"] `shouldBe` MkSubMovementPlus Forward 5 0
    it "given a downward direction and magnitude 1 should parse them" $ do
      toSubMovementPlus ["down", "1"] `shouldBe` MkSubMovementPlus Down 1 0
    it "given an upward direction and magnitude 7 should parse them" $ do
      toSubMovementPlus ["up", "7"] `shouldBe` MkSubMovementPlus Up 7 0

  describe "computing the aim" $ do
    it "given the empty list should leave the input unchanged" $ do
      computeAim [] `shouldBe` []
    it "given a single entry should set the aim to zero" $ do
      computeAim [MkSubMovementPlus Forward 1 2] `shouldBe` [MkSubMovementPlus Forward 1 0]
    it "given two entries should set the first aim to zero" $ do
      let movement = MkSubMovementPlus Forward 1 2
       in computeAim [movement, movement] `shouldBe` [MkSubMovementPlus Forward 1 0, movement]

-- it "given a second entry with direction down should increase its aim by its magnitude" $ do
--   let movement = MkSubMovementPlus Down 1 2
--    in computeAim [movement, movement] `shouldBe` [MkSubMovementPlus Forward 1 0, MkSubMovementPlus Down 1 3]

dayOneSampleData :: [String]
dayOneSampleData = ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]

dayTwoSampleData :: [String]
dayTwoSampleData = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]