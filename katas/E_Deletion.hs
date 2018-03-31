module E_Deletion where

import qualified Data.Map as Map

deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n =
  reverse
  . snd
  . foldl (\acc x -> let updatedFreq = Map.insertWith (+) x 1 (fst acc)
                         (Just freq) = Map.lookup x updatedFreq
                         deletedLst = snd acc
                         updatedLst =  if freq > n then deletedLst else x:deletedLst
                     in (updatedFreq, updatedLst))
          (Map.empty, [])
  $ lst
