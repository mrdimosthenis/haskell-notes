module A_One_to_Ten where

import Data.List
import Control.Arrow

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

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

compress :: (Eq a) => [a] -> [a]
compress = map head . group

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (frst,rest) = span (==x) xs
              in (x:frst) : pack rest

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (length &&& head) . pack
