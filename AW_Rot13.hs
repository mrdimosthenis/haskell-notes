module AW_Rot13 where

import Data.Char

base :: Char -> Int
base c
  | isUpper c = 65
  | isLower c = 97
  | otherwise =  error "there is no base for special characters"

shift :: Char -> Char
shift c = chr . (bs +) . (`mod` 26) . (13 +) . ((-bs) +) . ord $ c
  where bs = base c

rot13 :: String -> String
rot13 = map (\c -> if isLetter c && isAscii c then shift c else c)
