module R_IsHappy where

import Data.Char (digitToInt)

sumOfSquares :: Integer -> Integer
sumOfSquares = toInteger . sum . map (product . replicate 2 . digitToInt) . show

end :: [Integer] -> Integer
end (x:xs)
  | x `elem` xs = x
  | otherwise = end (sumOfSquares x : x : xs)
end _ = error "empty list"

isHappy :: Integer -> Bool
isHappy x = end [x] == 1
