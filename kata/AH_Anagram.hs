module AH_Anagram where

import qualified Data.Map as Map

frequencies :: String -> Map.Map Char Int
frequencies = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

anagrams :: String -> [String] -> [String]
anagrams w = filter (\s -> frequencies s == frequencies w)
