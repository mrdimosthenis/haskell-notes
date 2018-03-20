module AZ_PP where

range :: Integer -> Integer -> [Integer]
range a n = [2 .. floor (logBase (fromIntegral a) (fromIntegral n) :: Double) + 1]

isPP :: Integer -> Maybe (Integer, Integer)
isPP i = if null xs then Nothing else Just (head xs)
  where xs = [(a, b) | a <- [2..i], b <- filter (\x -> a ^ x == i) (range a i)]
