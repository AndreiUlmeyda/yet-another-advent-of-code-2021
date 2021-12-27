module Util (toNumberOfBase) where

type Radix = Int

toNumberOfBase :: Radix -> [Int] -> Int
toNumberOfBase radix = sum . zipWith (*) powersOfTen . reverse
  where
    powersOfTen = iterate (* radix) 1
