module A_Starting_Out where

penultimate :: [a] -> a
penultimate l = last (init l)

findK :: Int -> [a] -> a
findK k l = l !! k

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = reverse l == l

duplicate :: [a] -> [a]
duplicate xs = concat [[x, x] | x <- xs]

ziplike :: [a] -> [b] -> [(a,b)]
ziplike xs ys = [(xs!!i, ys!!i) | i <-[0..min (length xs) (length ys) - 1]]

ziplike' :: [a] -> [b] -> [(a,b)]
ziplike' _ [] = []
ziplike' [] _ = []
ziplike' (x:xs) (y:ys) = (x,y) : ziplike' xs ys

splitAtIndex :: Int -> [a] -> ([a],[a])
splitAtIndex k l = (take k l, drop k l)

dropK :: Int -> [a] -> [a]
dropK k l = take k l ++ drop (k + 1) l

slice :: Int -> Int -> [a] -> [a]
slice i k l = take (k - i) (drop i l)

insertElem :: a -> Int -> [a] -> [a]
insertElem x k l = take k l ++ [x] ++ drop k l

rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l
