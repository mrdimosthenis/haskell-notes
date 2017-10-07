module E_Higher_Order_Functions where

sumInts :: Int -> Int -> Int
sumInts a b = if a > b then 0
              else a + sumInts (a + 1) b

sq :: Int -> Int
sq x = x * x

sumSquares :: Int -> Int -> Int
sumSquares a b = if a > b then 0
                 else sq a + sumSquares (a + 1) b

higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b =
  if a > b then 0
  else intApplication a + higherOrderSum intApplication (a + 1) b

hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum sq

hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum id

higherOrderSequenceApplication :: Int -> (Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Int
higherOrderSequenceApplication a op f z b =
  if a > b then z
  else op a `f` higherOrderSequenceApplication (a + 1) op f z b

hoFactorial :: Int -> Int
hoFactorial = higherOrderSequenceApplication 1 id (*) 1
