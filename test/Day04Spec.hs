module Day04Spec (spec) where

import Day04
  ( Marking (Marked, UnMarked),
    playBingo,
    solutionDay4Part1,
    solutionDay4Part2,
    toWin,
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
  describe "playing bingo" $ do
    context "with neither numbers nor boards" $
      it "should result in Nothing" $ do
        playBingo toWin ([], []) `shouldBe` Nothing
    context "with no numbers but valid boards" $
      it "should result in Nothing" $ do
        playBingo toWin ([], [[[(0, UnMarked)], [(0, UnMarked)]]]) `shouldBe` Nothing
    context "with a number which completes the first board" $
      it "should return the current number and the board" $ do
        let almostCompleteBoard = [[(1, UnMarked)], [(2, Marked)]]
            completeBoard = [[(1, Marked)], [(2, Marked)]]
         in playBingo toWin ([1], [almostCompleteBoard]) `shouldBe` Just (1, completeBoard)
    context "with sample data for part 1" $
      it "should be 4512" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-04-sample"
        solutionDay4Part1 sampleData `shouldBe` 4512
    context "with actual data for part 1" $
      it "should be 50008" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-04"
        solutionDay4Part1 actualData `shouldBe` 50008
    context "with sample data for part 2" $
      it "should be 1924" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-04-sample"
        solutionDay4Part2 sampleData `shouldBe` 1924
    context "with actual data for part 2" $
      it "should be 17408" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-04"
        solutionDay4Part2 actualData `shouldBe` 17408
