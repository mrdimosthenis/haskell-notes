module AL_RomanNumerals where

import Data.Char (digitToInt)

solution :: Integer -> String
solution n = ["", "M", "MM", "MMM"] !! (placeValues !! 3)
  ++ ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"] !! (placeValues !! 2)
  ++ ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"] !! (placeValues !! 1)
  ++ ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] !! head placeValues
  where placeValues = map digitToInt . reverse $ "000" ++ show n
