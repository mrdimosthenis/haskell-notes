module C_Syntax_in_functions where

englishDigit :: Int -> String
englishDigit x
   | x >= 0 && x <= 9 = digits !! x
   | otherwise        = "unknown"
   where digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "unknown"]

divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (_, 0) = undefined
divTuple (x, y) = x / y

threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList _ = False
