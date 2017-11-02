module Exercises.AI_Parentheses where

charValue :: Char -> Int
charValue '(' = 1
charValue ')' = negate 1
charValue _ = 0

validParentheses :: String -> Bool
validParentheses s = all (>= 0) (init sumsOfValues) && (last sumsOfValues == 0)
  where sumsOfValues = scanl (\acc x -> acc + charValue x) 0 s
