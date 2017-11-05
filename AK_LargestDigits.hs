module AK_LargestDigits where

import Data.List (tails)

digit5 :: String -> Int
digit5 = maximum . map ((\x -> read x::Int) . take 5) . init . tails
