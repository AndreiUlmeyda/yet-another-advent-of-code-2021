module Day10Spec (spec) where

import Day10
  ( firstCorruptCharacter,
    solutionDay10Part1,
    solutionDay10Part2,
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
  describe "scoring corrupted lines" $ do
    context "with sample data for part 1" $
      it "should result in 26397" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-10-sample"
        solutionDay10Part1 sampleData `shouldBe` 26397
    context "with actual data for part 1" $
      it "should result in 311949" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-10"
        solutionDay10Part1 actualData `shouldBe` 311949

  describe "completing, then scoring lines" $ do
    context "with sample data for part 2" $
      it "should result in x" $ do
        sampleData <- lines <$> readFile "puzzle-inputs/day-10-sample"
        solutionDay10Part2 sampleData
          `shouldBe` 288957
    context "with actual data for part 2" $
      it "should result in x" $ do
        actualData <- lines <$> readFile "puzzle-inputs/day-10"
        solutionDay10Part2 actualData `shouldBe` 3042730309

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
  context "given an opening bracket followed by a non matching closing bracket" $
    it "should result in the closing bracket" $ do
      firstCorruptCharacter "(]" `shouldBe` Just ']'
  context "given a pair of matched brackets" $
    it "should result in Nothing" $ do
      firstCorruptCharacter "()" `shouldBe` Nothing
  context "given a pair of matched brackets followed by a different opening bracket" $
    it "should result in Nothing" $ do
      firstCorruptCharacter "()<" `shouldBe` Nothing
  context "given a missing closing bracket with otherwise matching ones" $
    it "should result in Nothing" $ do
      firstCorruptCharacter "({<>}" `shouldBe` Nothing
  context "given nested pairs of matched brackets" $
    it "should result in Nothing" $ do
      firstCorruptCharacter "({<>})" `shouldBe` Nothing
  context "given sample line 1" $
    it "should result in }" $ do
      firstCorruptCharacter "{([(<{}[<>[]}>{[]{[(<()>" `shouldBe` Just '}'
  context "given sample line 2" $
    it "should result in )" $ do
      firstCorruptCharacter "[[<[([]))<([[{}[[()]]]" `shouldBe` Just ')'
  context "given sample line 3" $
    it "should result in ]" $ do
      firstCorruptCharacter "[{[{({}]{}}([{[{{{}}([]" `shouldBe` Just ']'
  context "given sample line 4" $
    it "should result in )" $ do
      firstCorruptCharacter "[<(<(<(<{}))><([]([]()" `shouldBe` Just ')'
  context "given sample line 5" $
    it "should result in >" $ do
      firstCorruptCharacter "<{([([[(<>()){}]>(<<{{" `shouldBe` Just '>'
