module K_LargestDigits where

import Data.List (tails)

digit5 :: String -> Int
digit5 = maximum . map (read . take 5) . init . tails
