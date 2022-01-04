module Util (toNumberOfBase) where

type Radix = Int

toNumberOfBase :: Radix -> [Int] -> Int
toNumberOfBase radix = sum . zipWith (*) powersOfRadix . reverse
  where
    powersOfRadix = iterate (* radix) 1
