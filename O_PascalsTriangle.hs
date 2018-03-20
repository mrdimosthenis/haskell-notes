module O_PascalsTriangle where

pascalsTriangle :: Integer -> [Integer]
pascalsTriangle x = [binCoefficient i j | i <- [0..x-1], j <- [0..i]]
  where binCoefficient n k = factorial n `div` (factorial k * factorial (n - k))
        factorial 0 = 1
        factorial n = n * factorial (n - 1)
