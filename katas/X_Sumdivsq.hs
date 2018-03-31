module X_Sumdivsq where

import Data.Ratio ((%))

intSqrt :: Int -> Int
intSqrt x = truncate $ sqrt (fromIntegral x :: Double)

isSquare :: Int -> Bool
isSquare x = let sqr = intSqrt x
             in sqr * sqr == x

sumOfSquaredDivisors :: Int -> Int
sumOfSquaredDivisors n
  = sum
  . map (\x -> x * x)
  . concatMap (\i -> if n % i == fromIntegral i then [i] else [i, n `div` i])
  . filter (\i -> n `mod` i == 0)
  $ [1 .. intSqrt n]

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = filter (isSquare . snd)
                . map (\x -> (x, sumOfSquaredDivisors x))
                $ [m..n]
