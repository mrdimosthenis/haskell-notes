module Slides.A_Basics where

import Numeric.Natural
import Data.Function
import Data.Char

addMul :: Int -> Int -> Int -> Int
addMul x y z = x + y * z

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

{-|
emptyList     :: [Int]
emptyList      = []
-}

listExample   :: [Int]
listExample    = [2, 1, 3]

singletonList :: [Int]
singletonList  = 1 : emptyList

listExample' :: [Int]
listExample'   = 5:10:listExample

twoLists :: [Int]
twoLists       = singletonList ++ listExample

trinity :: [Int]
trinity        = listExample ++ [7] ++ twoLists

string        :: [Char]
string         = "str"

otherString   :: String
otherString    = "other " ++ string

{-|
pythagoras :: Int -> Int -> Int
pythagoras x y = let x2 = x ^ 2
                     y2 = y ^ 2
                 in x2 + y2
-}

pythagoras :: Double -> Double -> Double
pythagoras a b = a2 + b2
  where
    square x = x ^ 2
    a2       = square a
    b2       = square b

factorial :: Integer -> Integer
factorial n = if n <= 1
              then 1
              else n * factorial (n - 1)

collatzSum :: Natural -> Natural
collatzSum n
    | n < 0     = 0
    | n == 1    = 1
    | even n    = n + collatzSum (n `div` 2)
    | otherwise = n + collatzSum (3 * n + 1)

getFont :: Int -> String
getFont n = case n of
        0 -> "PLAIN"
        1 -> "BOLD"
        2 -> "ITALIC"
        _ -> "UNKNOWN"

caseOperation :: Char -> Int -> Int -> Int
caseOperation op x y =
    case op of
         '+' -> x + y
         '-' -> x - y
         _   -> 0

inc, dec :: Int -> Int
inc x = x + 1
dec x = x - 1

changeTwiceBy :: (Int -> Int) -> Int -> Int
changeTwiceBy operation value = operation (operation value)

{-|
tripleApply :: (Int -> Int -> Int) -> Int -> Int
tripleApply (.+.) x = (x .+. x) .+. (x .+. x)
-}

emptyList :: [a]
emptyList = []

repeatThree :: a -> [a]
repeatThree x = [x, x, x]

tripleApply :: (a -> a -> a) -> a -> a
tripleApply f x = f (f x x) (f x x)

spaceSumConcat :: (String, Int, Int) -> String
spaceSumConcat(s, x, y) = s ++ " " ++ show(x + y)

{-|
foo, bar :: Int -> String
foo x = show x
bar   = show
-}

div7By :: Int -> Int
div7By = div (7 :: Int)

show2 :: Int -> Int -> String
show2 x y = show x ++ " and " ++ show y

showSnd , showFst, showFst' :: Int -> String
showSnd = show2 1
showFst = flip show2 2
showFst' = (`show2` 2)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

stringLit :: String -> String
stringLit "such" = "pattern"
stringLit "much" = "amaze"
stringLit x      = "wow"

sumList3 :: [Int] -> Int
sumList3 [x, y, z] = x + y + z
sumList3 _         = 0

puzzle :: [Int] -> [Int]
puzzle (x:y:z:ls) = if x < y && y > z then y : c else c
  where c = puzzle (y:z:ls)
puzzle _ = []

{-|
foo, bar :: [Int] -> Int
foo list = length (filter odd (map (div 2) (filter even (map (div 7) list))))
bar list = length $ filter odd $ map (div 2) $ filter even $ map (div 7) list
-}

incNegate :: Int -> Int
--incNegate x = negate (x + 1)
--incNegate x = negate $ x + 1
--incNegate x = (negate . (+1)) x
--incNegate x = negate . (+1) $ x
incNegate = negate . (+1)

foo, bar :: [Int] -> Int
foo patak = length $ filter odd $ map (div 2) $ filter even $ map (div 7) patak
bar       = length . filter odd . map (div 2) . filter even . map (div 7)

stringTransform :: [String] -> [String]
--stringTransform l = map (\s -> map toUpper s) (filter (\s -> length s == 5) l)
--stringTransform l = map (\s -> map toUpper s) $ filter (\s -> length s == 5) l
--stringTransform l = map (map toUpper) $ filter ((== 5) . length) l
stringTransform = map (map toUpper) . filter ((== 5) . length)

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs)
  = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]
