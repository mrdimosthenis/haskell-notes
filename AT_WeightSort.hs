module AT_WeightSort where

import Data.Char (digitToInt)
import Data.List (sortBy)

calcWeight :: String -> Int
calcWeight = sum . map digitToInt

comp :: String -> String -> Ordering
x `comp` y
  | calcWeight x < calcWeight y = LT
  | calcWeight y < calcWeight x = GT
  | otherwise = x `compare` y

orderWeight :: String -> String
orderWeight = unwords . sortBy comp . words
