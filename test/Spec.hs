import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  -- describe "day 1 counting the number of times the measurement increases" $ do
  --   it "given the empty list should return zero" $ do
  --     day01CountIncreases ([] :: [Int]) `shouldBe` 0
  --   it "given a single entry should return return zero" $ do
  --     day01CountIncreases [1] `shouldBe` 0
  --   it "given two equal entries should return zero" $ do
  --     day01CountIncreases [1, 1] `shouldBe` 0
  describe "generating a list of differences" $ do
    it "given the empty list should return the input unchanged" $ do
      listOfDifferences ([] :: [Int]) `shouldBe` []
    it "given one entry, with nothing to compare, should return the empty list" $ do
      listOfDifferences [1] `shouldBe` []
    it "given two entries should return a single entry list containing their difference" $ do
      listOfDifferences [1, 4] `shouldBe` [3]
    it "given many entries should return a list containing their differences" $ do
      listOfDifferences [3, 7, 1, 2, 15, 5] `shouldBe` [4, -6, 1, 13, -10]