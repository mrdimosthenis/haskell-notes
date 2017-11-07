module AN_BreakCamelCase where

import Data.Char (isUpper)

solution :: String -> String
solution s = take 1 s ++ match (tail s)
  where match [] = []
        match (x:xs)
          | isUpper x = ' ' : x : match xs
          | otherwise = x : match xs
