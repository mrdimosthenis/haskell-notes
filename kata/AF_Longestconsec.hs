module AF_Longestconsec where

import Data.List (maximumBy)
import Data.Function (on)

longestConsec :: [String] -> Int -> String
longestConsec strarr k
  | k > n || k <= 0 = ""
  | otherwise = maximumBy (compare `on` length)
                . reverse
                . map (\i -> concat . take k . drop i $ strarr)
                $ [0..n-1]
  where n = length strarr
