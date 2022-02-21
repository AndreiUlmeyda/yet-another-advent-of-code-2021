module Day10Spec (spec) where

import Day10
  ( -- solutionDay10Part2,
    firstCorruptCharacter,
    solutionDay10Part1,
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
  describe "" $ do
    context "with sample data for part 1" $
      it "should result in x" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-10-sample"
        solutionDay10Part1 sampleData `shouldBe` 26397

  context "with actual data for part 1" $
    it "should result in x" $ do
      actualData <- lines <$> readFile "puzzle-inputs/day-10"
      solutionDay10Part1 actualData `shouldBe` 0

  describe "finding the first corrupt character" $ do
    context "given the empty string" $
      it "should result in Nothing" $ do
        firstCorruptCharacter "" `shouldBe` Nothing
  context "given a single unmatched bracket" $
    it "should result in Nothing, since only an incorrect closing character counts as corrupt" $ do
      firstCorruptCharacter "(" `shouldBe` Nothing
  context "given a single closing bracket" $
    it "should result just that closing braket" $ do
      firstCorruptCharacter ")" `shouldBe` Just ')'
  context "given a differing closing bracket" $
    it "should result in the closing bracket" $ do
      firstCorruptCharacter "(]" `shouldBe` Just ']'
  context "given a matching bracket which is outside of the current chunk" $
    it "should result in an unmatched bracket" $ do
      firstCorruptCharacter "(<)>" `shouldBe` Just ')'
  context "given a pair of matched brackets" $
    it "should result in Nothing" $ do
      firstCorruptCharacter "()" `shouldBe` Nothing
  context "given a pair of matched brackets followed by a different opening bracket" $
    it "should result in Nothing" $ do
      firstCorruptCharacter "()<" `shouldBe` Nothing
  context "given a matching bracket which is outside of the current chunk" $
    it "should result in an unmatched bracket" $ do
      firstCorruptCharacter "({}()<)>" `shouldBe` Just ')'
  context "given " $
    it "should result in " $ do
      firstCorruptCharacter "({<>}" `shouldBe` Nothing
  context "given nested pairs of matched brackets" $
    it "should result in Nothing" $ do
      firstCorruptCharacter "({<>})" `shouldBe` Nothing
  context "given TODO" $
    it "should result in TODO" $ do
      firstCorruptCharacter "{([(<{}[<>[]}>{[]{[(<()>" `shouldBe` Just '}'
  context "given TODO" $
    it "should result in TODO" $ do
      firstCorruptCharacter "[[<[([]))<([[{}[[()]]]" `shouldBe` Just ')'
  context "given TODO" $
    it "should result in TODO" $ do
      firstCorruptCharacter "[{[{({}]{}}([{[{{{}}([]" `shouldBe` Just ']'
  context "given TODO" $
    it "should result in TODO" $ do
      firstCorruptCharacter "[<(<(<(<{}))><([]([]()" `shouldBe` Just ')'
  context "given TODO" $
    it "should result in TODO" $ do
      firstCorruptCharacter "<{([([[(<>()){}]>(<<{{" `shouldBe` Just '>'

-- context "with sample data for part 2" $
--   it "should result in 1134" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-09-sample"
--     solutionDay9Part2 sampleData
--       `shouldBe` 1134
-- context "with actual data for part 2" $
--   it "should result in 949905" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-09"
--     solutionDay9Part2 actualData `shouldBe` 949905