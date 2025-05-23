module Main where

import Task1 (mreverse)
import Task2 (mulTable)
import Task3 (countOldest)

main :: IO ()
main = do
  
  putStrLn "Enter a string to reverse:"
  input <- getLine
  print $ mreverse input

  putStrLn "\nMultiplication Table:"
  nr <- readLn
  mulTable nr

