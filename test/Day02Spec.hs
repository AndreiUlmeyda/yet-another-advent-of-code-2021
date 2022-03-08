module Day02Spec (spec) where

import Day02
  ( SubDirection (Down, Forward, Up),
    SubMovementPlus (MkSubMovementPlus),
    computeAim,
    solutionDay2Part1,
    solutionDay2Part2,
    sumDistancesConsideringAim,
    toSubMovementPlus,
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
  )

-- runtime: 10ms - 20ms
spec :: Spec
spec = do
  describe "computing a final position from steering commands" $ do
    context "with sample data for part 1" $
      it "should be 150" $ do
        solutionDay2Part1 dayTwoSampleData `shouldBe` 150
    context "with actual data for part 1" $
      it "should be 1727835" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-02"
        solutionDay2Part1 actualData `shouldBe` 1727835
    context "with sample data for part 2" $
      it "should be 900" $ do
        solutionDay2Part2 dayTwoSampleData `shouldBe` 900
    context "with actual data for part 2" $
      it "should be 1544000595" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-02"
        solutionDay2Part2 actualData `shouldBe` 1544000595

  describe "parsing sub movements" $ do
    context "given a forward direction and magnitude 5" $
      it "should parse them" $ do
        toSubMovementPlus ["forward", "5"] `shouldBe` MkSubMovementPlus Forward 5 0
    context "given a downward direction and magnitude 1" $
      it " should parse them" $ do
        toSubMovementPlus ["down", "1"] `shouldBe` MkSubMovementPlus Down 1 0
    context "given an upward direction and magnitude 7" $
      it "should parse them" $ do
        toSubMovementPlus ["up", "7"] `shouldBe` MkSubMovementPlus Up 7 0

  describe "computing the aim" $ do
    context "given the empty list" $
      it "should leave the input unchanged" $ do
        computeAim [] `shouldBe` []
    context "given a second entry with direction down" $
      it "should result in an aim which is the sum of the previous aim and the current magnitude" $ do
        computeAim [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Down 3 2] `shouldBe` [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Down 3 9]
    context "given a second entry with direction forward" $
      it "should keep the aim of the first entry" $ do
        computeAim [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Forward 3 0] `shouldBe` [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Forward 3 6]
    context "given a second entry with direction up" $
      it "should result in an aim where the current magnitude is subtracted from the previous aim" $ do
        computeAim [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Up 3 2] `shouldBe` [MkSubMovementPlus Forward 1 6, MkSubMovementPlus Up 3 3]

  describe "computing the position considering aim" $ do
    context "given an empty list of movements" $
      it "should return the starting position" $ do
        sumDistancesConsideringAim (0, 0) [] `shouldBe` (0, 0)
    context "given one movement forward without aim" $
      it "should add x positions" $ do
        sumDistancesConsideringAim (0, 0) [MkSubMovementPlus Forward 1 0] `shouldBe` (1, 0)
    context "given one movement forward the ai" $
      it "should also modify y position by magnitude times aim" $ do
        sumDistancesConsideringAim (0, 0) [MkSubMovementPlus Forward 2 3] `shouldBe` (2, 6)

dayTwoSampleData :: [String]
dayTwoSampleData =
  [ "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  ]