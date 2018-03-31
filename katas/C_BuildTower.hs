module C_BuildTower where

buildFloor :: Int -> Int -> String
buildFloor i n = let spaces = replicate (n - i) ' '
                     asterisks = replicate (2*i - 1) '*'
                 in spaces ++ asterisks ++ spaces

buildTower :: Int -> [String]
buildTower n = map (`buildFloor` n) [1..n]
