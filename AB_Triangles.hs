module AB_Triangles where

import Data.List (sort)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = let [x, y, z] = sort [a, b, c]
                   in x + y > z
