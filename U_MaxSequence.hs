module U_MaxSequence where

import Data.List (tails,inits)

maxSequence :: [Int] -> Int
maxSequence xs
  | null xs || all (<0) xs = 0
  | otherwise = maximum . concatMap (map sum . tail . inits) . tails $ xs
