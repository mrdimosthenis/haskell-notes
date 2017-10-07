module D_Recursion where

power :: Int -> Int -> Int
power _ 0 = 1
power x y = x * power x (y - 1)

fib :: (Num a, Eq a) => a -> [a]
fib 0 = [0]
fib 1 = [1, 0]
fib n = (a + b) : l
   where l@(a:b:_) = fib (n - 1)
