import Day01
  ( solutionDay1Part1,
    solutionDay1Part2,
  )
import Day02
  ( SubDirection (Down, Forward, Up),
    SubMovementPlus (MkSubMovementPlus),
    computeAim,
    solutionDay2Part1,
    solutionDay2Part2,
    sumDistancesConsideringAim,
    toSubMovementPlus,
  )
import Day03
  ( addElementwise,
    solutionDay3Part1,
    solutionDay3Part2,
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
    it "the solution to part two with sample data should be 900" $ do
      solutionDay2Part2 dayTwoSampleData `shouldBe` 900 -- full data result 1544000595
  describe "day 3" $ do
    it "the solution to part one with sample data should be 198" $ do
      solutionDay3Part1 dayThreeSampleData `shouldBe` 198 -- full data result
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

    it "given a second entry with direction down should result in an aim which is the sum of the previous aim and the current magnitude" $ do
      computeAim [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Down 3 2] `shouldBe` [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Down 3 9]

    it "given a second entry with direction forward should keep the aim of the first entry" $ do
      computeAim [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Forward 3 0] `shouldBe` [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Forward 3 6]

    it "given a second entry with direction up should result in an aim where the current magnitude is subtracted from the previous aim" $ do
      computeAim [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Up 3 2] `shouldBe` [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Up 3 3]

  describe "computing the position considering aim" $ do
    it "given an empty list of movements should return the starting position" $ do
      sumDistancesConsideringAim (0, 0) [] `shouldBe` (0, 0)

    it "given one movement forward without aim should add x positions" $ do
      sumDistancesConsideringAim (0, 0) [MkSubMovementPlus Forward 1 0] `shouldBe` (1, 0)

    it "given one movement forward the aim should also modify y position by magnitude times aim" $ do
      sumDistancesConsideringAim (0, 0) [MkSubMovementPlus Forward 2 3] `shouldBe` (2, 6)

  describe "adding diagnostic numbers" $ do
    it "when one number is zero, the result should be equal to the other one" $
      do
        addElementwise [0, 0, 0] [1, 1, 1]
        `shouldBe` [1, 1, 1]
    it "each element of the first number should be added to the corresponding element of the second one" $
      do
        addElementwise [1, 2, 999] [-100, 2, 1]
        `shouldBe` [-99, 4, 1000]

dayOneSampleData :: [String]
dayOneSampleData = ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]

dayTwoSampleData :: [String]
dayTwoSampleData = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

dayThreeSampleData :: [String]
dayThreeSampleData =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]