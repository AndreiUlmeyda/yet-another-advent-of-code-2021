module Day12Spec (spec) where

import Day12
  ( Connection (MkConnection),
    generatePaths,
    mkConnection,
    solutionDay12Part1,
    solutionDay12Part2,
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
  -- describe "counting paths" $ do
    -- context "with sample data for part 1" $
    --   it "should result in x" $ do
    --     sampleData <- lines <$> readFile "puzzle-inputs/day-12-sample"
    --     solutionDay12Part1 sampleData `shouldBe` []
  describe "generating paths" $ do
    context "given an empty list of connections" $
      it "should result in an empty list of paths" $ do
        generatePaths [] `shouldBe` []
    -- context "given a single connection" $
    --   it "should result a single path containing that connection" $ do
    --     let simpleConnection = mkConnection ["start", "end"]
    --     generatePaths [simpleConnection] `shouldBe` [[simpleConnection]]
    -- context "given two connections with a shared cave" $
    --   it "should result in one path containing the two connections" $ do
    --     let connection1 = mkConnection ["start", "a"]
    --     let connection2 = mkConnection ["a", "end"]
    --     generatePaths [connection1, connection2] `shouldBe` [[connection1, connection2]]
    -- context "given two connections without a common cave" $
    --   it "should result in two paths of length one" $ do
    --     let connection1 = mkConnection ["start", "a"]
    --     let connection2 = mkConnection ["b", "end"]
    --     generatePaths [connection1, connection2] `shouldBe` [[connection1], [connection2]]

-- context "with actual data for part 1" $
--   it "should result in x" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-12"
--     solutionDay12Part1 actualData `shouldBe` 1
-- context "with sample data for part 2" $
--   it "should result in x" $ do
--     sampleData <- lines <$> readFile "puzzle-inputs/day-12-sample"
--     solutionDay12Part2 sampleData `shouldBe` 2
-- context "with actual data for part 2" $
--   it "should result in x" $ do
--     actualData <- lines <$> readFile "puzzle-inputs/day-12"
--     solutionDay12Part2 actualData `shouldBe` 2