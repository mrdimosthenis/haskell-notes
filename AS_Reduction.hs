module AS_Reduction where

data Direction = North | East | West | South deriving (Eq, Show)

match :: [Direction] -> Direction -> [Direction]
match (North:xs) South = xs
match (South:xs) North = xs
match (East:xs) West = xs
match (West:xs) East = xs
match acc x = x : acc

dirReduce :: [Direction] -> [Direction]
dirReduce = reverse . foldl match []
