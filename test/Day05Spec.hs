module Day05Spec (spec) where

import Day05
  ( Line (MkLine),
    listOfCoordinatesFrom,
    range,
    solutionDay5Part1,
    solutionDay5Part2,
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
  )

-- runtime: 280ms - 300ms
spec :: Spec
spec = do
  describe "avoiding hydrothermal vents" $ do
    context "with sample data for part 1" $
      it "should result in 5" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-05-sample"
        solutionDay5Part1 sampleData `shouldBe` 5
    context "with actual data for part 1" $
      it "should result in 5774" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-05"
        solutionDay5Part1 actualData `shouldBe` 5774
    context "with sample data for part 2" $
      it "should result in 12" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-05-sample"
        solutionDay5Part2 sampleData `shouldBe` 12
    context "with actual data for part 2" $
      it "should result in 18423" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-05"
        solutionDay5Part2 actualData `shouldBe` 18423
  describe "generating lists of coordinates from line start and end coordinates" $ do
    context "given a line where start and end coincide" $
      it "should produce that single coordinate" $ do
        listOfCoordinatesFrom (MkLine 1 1 1 1) `shouldBe` [(1, 1)]
    context "given a horizontal line" $
      it "should list the coordinates which have a constant x component" $ do
        listOfCoordinatesFrom (MkLine 1 1 1 3) `shouldBe` [(1, 1), (1, 2), (1, 3)]
    context "given a vertical line" $
      it "should list the coordinates which have a constant y component" $ do
        listOfCoordinatesFrom (MkLine 1 1 3 1) `shouldBe` [(1, 1), (2, 1), (3, 1)]
    context "given a diagonal line" $
      it "should list the coordinates with x and y varying constantly" $ do
        listOfCoordinatesFrom (MkLine 0 0 2 2) `shouldBe` [(0, 0), (1, 1), (2, 2)]
    context "given a diagonal line with negative y direction" $
      it "should list the coordinates with x and y varying constantly" $ do
        listOfCoordinatesFrom (MkLine 0 0 2 (-2)) `shouldBe` [(0, 0), (1, -1), (2, -2)]
  describe "generating a range" $ do
    context "given identical start and end" $
      it "should generate a single entry" $ do
        range 0 0 `shouldBe` ([0] :: [Int])
    context "given a start and a bigger end" $
      it "should generate a list incrementing by 1" $ do
        range 0 3 `shouldBe` ([0, 1, 2, 3] :: [Int])
    context "given a start and a smaller end" $
      it "should generate a list decrementing by 1" $ do
        range 2 0 `shouldBe` ([2, 1, 0] :: [Int])
