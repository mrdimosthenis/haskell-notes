module AV_Fib where

initPair :: (Integer, Integer)
initPair = (1, 1)

nextPair :: (Integer, Integer) -> (Integer, Integer)
nextPair (x, y) = (y, x + y)

isProdEnough :: (Integer, Integer) -> Integer -> Bool
isProdEnough (x, y) n = x * y >= n

productFib :: Integer -> (Integer, Integer, Bool)
productFib n = let (x, y) = until (`isProdEnough` n) nextPair initPair
               in (x, y, x * y == n)
