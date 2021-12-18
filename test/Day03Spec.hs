module Day03Spec (spec) where

import Day03 (addElementwise, filterDiagnosticNumbers, solutionDay3Part1, solutionDay3Part2)
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "sample data solutions" $ do
    context "part one" $
      it "should be 198" $ do
        solutionDay3Part1 dayThreeSampleData `shouldBe` 198 -- full data result 3309596
    context "part one with different sample data" $
      it "should be 3879216" $ do
        solutionDay3Part1 dayThreeSampleData2 `shouldBe` 3879216
  -- context "herp" $
  --   it "derp" $ do
  --     solutionDay3Part2 dayThreeSampleData `shouldBe` []

  describe "adding diagnostic numbers" $ do
    context "when one number is zero" $
      it "should be equal to the other one" $
        do
          addElementwise [0, 0, 0] [1, 1, 1]
          `shouldBe` [1, 1, 1]
    context "given two lists" $
      it "each element of the first number should be added to the corresponding element of the second one" $
        do
          addElementwise [1, 2, 999] [-100, 2, 1]
          `shouldBe` [-99, 4, 1000]

  describe "filtering diagnostic numbers" $ do
    context "" $
      it "should do..." $ do
        filterDiagnosticNumbers [[1, 0], [1, 1], [0, 1]] `shouldBe` []

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

dayThreeSampleData2 :: [String]
dayThreeSampleData2 =
  [ "111010000101",
    "001001110011",
    "101001101111",
    "101110101110",
    "101000101111"
    --   101000101111 binary -> 2607 decimal
    --   010111010000 binary -> 1488 decimal
    --  product -> 3.879216*10^7
  ]