module L_Doubles where

  doubles :: Int -> Int -> Double
  doubles maxk maxn = sum
    [1/(fromIntegral k * (fromIntegral n + 1) ^ (2 * k)) | k <- [1..maxk], n <- [1..maxn]]
