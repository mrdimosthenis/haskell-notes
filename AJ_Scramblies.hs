module AJ_Scramblies where

import Data.List ((\\))

scramble :: String -> String -> Bool
scramble s1 s2 = s2 \\ s1 == ""
