module AQ_MultNumAsStrings where

multiply :: String -> String -> String
multiply xs ys = show $ parse xs * parse ys
  where parse x = read x :: Integer
