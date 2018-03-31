module P_RectangleRot where

rotate :: (Float, Float) -> (Float, Float)
rotate (x, y) = (x * cos v - y * sin v, x * sin v + y * cos v)
  where v = negate pi / 4

isInner :: (Float, Float) -> (Float, Float) -> Bool
isInner (x, y) (w, h)
  | abs rotX <= w && abs rotY <= h = True
  | otherwise = False
  where (rotX, rotY) = rotate (x, y)

rectangleRot :: Int -> Int -> Int
rectangleRot a b = let (w, h) = (fromIntegral a / 2, fromIntegral b / 2)
                       r = max w h * 2
                       range = [(-r)..r]
                   in length
                    . filter (`isInner` (w, h))
                    $ [(x, y) | x <- range, y <- range]
