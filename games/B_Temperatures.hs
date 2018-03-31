module B_Temperatures where

import System.IO
import Control.Monad

main :: IO ()
main = do
      hSetBuffering stdout NoBuffering
      numOfTempratures <- getLine
      input_line <- getLine
      let tempratures = map (\x -> read x :: Int) $ words input_line

      if numOfTempratures == "0"
        then putStrLn "0"
        else print $ foldl1 minTempr tempratures

      return ()

minTempr :: Int -> Int -> Int
minTempr x y
  | abs x <  abs y = x
  | abs x >  abs y = y
  | abs x == abs y = if x > y then x else y
