module A_One_to_Ten where

myLast :: [a] -> a
myLast [] = error "No last in empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast = last . init

elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs
