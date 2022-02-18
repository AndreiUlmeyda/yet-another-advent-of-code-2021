module Day10Spec (spec) where

import Day10
  ( -- solutionDay10Part2,

    Chunk (MkChunk, MkInCompleteChunk),
    contentOfFirstBracket,
    firstCorruptCharacter,
    parseChunks,
    solutionDay10Part1,
    upUntilNthCharacter,
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
  describe "get contents of the first bracket" $ do
    context "given the empty string" $
      it "should result in Nothing" $ do
        contentOfFirstBracket "" `shouldBe` Nothing
    context "given a single opening bracket" $
      it "should result in Nothing" $ do
        contentOfFirstBracket "(" `shouldBe` Nothing
    context "given two matching brackets" $
      it "should result in the empty string" $ do
        contentOfFirstBracket "()" `shouldBe` Just ""
    context "given characters inbetween two matched brackets" $
      it "should result in just those characters" $ do
        contentOfFirstBracket "(something)" `shouldBe` Just "something"
    context "given another opening bracket of the same type" $
      it "should consider that bracket as being associated with the inner bracket" $ do
        contentOfFirstBracket "((something)" `shouldBe` Just "(something)"
    context "given another inner pair of matched brackets of the same type" $
      it "should return the inner brackets" $ do
        contentOfFirstBracket "(())" `shouldBe` Just "()"

  describe "up until the nth character" $ do
    context "given the empty string" $
      it "should result in the empty string" $ do
        upUntilNthCharacter 1 'a' "" `shouldBe` []
    context "given a string without the relevant character" $
      it "should result in the complete string" $ do
        upUntilNthCharacter 1 'a' "bc" `shouldBe` "bc"
    context "given the relevant character and count 1" $
      it "should not contain the relevant character" $ do
        upUntilNthCharacter 1 'a' "bcad" `shouldBe` "bca"
    context "extracting up until the second occurence" $
      it "should ignore the first one" $ do
        upUntilNthCharacter 2 'a' "bcadaw" `shouldBe` "bcada"
    context "extracting up until the third occurence" $
      it "should should ignore the first two" $ do
        upUntilNthCharacter 3 'a' "bcadawrap" `shouldBe` "bcadawra"
    context "given a string containing that character" $
      it "should include it" $ do
        upUntilNthCharacter 1 ')' "()" `shouldBe` "()"

  describe "parsing chunks" $ do
    context "given the empty string" $
      it "should result in an empty list" $ do
        parseChunks "" `shouldBe` []
    context "given a single opening bracket" $
      it "should result in an incomplete chunk" $ do
        parseChunks "(" `shouldBe` [MkInCompleteChunk '(' []]
    context "given a matched bracket" $
      it "should result in a complete chunk with correct characters" $ do
        parseChunks "()" `shouldBe` [MkChunk '(' ')' []]
    context "given two matched brackets in a row" $
      it "should result in a list of two complete chunks with correct characters" $ do
        parseChunks "()()" `shouldBe` [MkChunk '(' ')' [], MkChunk '(' ')' []]
    context "given two nested brackets, each of them matched" $
      it "should result in a chunk inside of the 'innerChunks' of the first" $ do
        parseChunks "(<>)" `shouldBe` [MkChunk '(' ')' [MkChunk '<' '>' []]]
    context "given " $
      it "should result in " $ do
        parseChunks "(<)" `shouldBe` [MkChunk '(' ')' [MkInCompleteChunk '<' []]]

  -- describe "" $ do
  --   context "with sample data for part 1" $
  --     it "should result in x" $ do
  --       sampleData <- lines <$> readFile "puzzle-inputs/day-10-sample"
  --       solutionDay10Part1 sampleData `shouldBe` []
  describe "finding the first corrupt character" $ do
    context "given the empty string" $
      it "should result in Nothing" $ do
        firstCorruptCharacter "" `shouldBe` Nothing
    context "given a single unmatched bracket" $
      it "should result in Just that character" $ do
        firstCorruptCharacter "(" `shouldBe` Just '('
    context "given a pair of matched brackets" $
      it "should result in Nothing" $ do
        firstCorruptCharacter "()" `shouldBe` Nothing
    context "given a pair of matched brackets folloew by a different single bracket" $
      it "should result in the different bracket" $ do
        firstCorruptCharacter "()<" `shouldBe` Just '<'
    context "given nested pairs of matched brackets" $
      it "should result in Nothing" $ do
        firstCorruptCharacter "({<>})" `shouldBe` Nothing
    context "given a matching bracket which is outside of the current chunk" $
      it "should result an unmatched bracket" $ do
        firstCorruptCharacter "(<)>" `shouldBe` Just '<'

-- context "given a matching bracket which is outside of the current chunk" $
--   it "should result an unmatched bracket" $ do
--     firstCorruptCharacter "({}()<)>" `shouldBe` Just '<'

-- context "with actual data for part 1" $
--   it "should result in 518" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-09"
--     solutionDay9Part1 actualData `shouldBe` 518
-- context "with sample data for part 2" $
--   it "should result in 1134" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-09-sample"
--     solutionDay9Part2 sampleData
--       `shouldBe` 1134
-- context "with actual data for part 2" $
--   it "should result in 949905" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-09"
--     solutionDay9Part2 actualData `shouldBe` 949905