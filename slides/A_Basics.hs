module A_Basics where

import Numeric.Natural

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

id :: a -> a
id x = x

emptyList :: [a]
emptyList = []

repeatThree :: a -> [a]
repeatThree x = [x, x, x]

tripleApply :: (a -> a -> a) -> a -> a
tripleApply f x = f (f x x) (f x x)

spaceSumConcat :: (String, Int, Int) -> String
spaceSumConcat(s, x, y) = s ++ " " ++ show(x + y)

foo, bar :: Int -> String
foo x = show x
bar   = show

div7By :: Int -> Int
div7By = div (7 :: Int)
