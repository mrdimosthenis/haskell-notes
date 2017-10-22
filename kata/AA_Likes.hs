module AA_Likes where

likes :: [String] -> String
likes []       = "no one likes this"
likes [x]      = x ++ " likes this"
likes [x,y]    = x ++ " and " ++ y ++ " like this"
likes [x,y,z]  = x ++ ", " ++ y ++ " and " ++ z ++ " like this"
likes (x:y:xs) = x ++ ", " ++ y ++ " and " ++ show (length xs) ++ " others like this"
