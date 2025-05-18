module Main (main) where

import Task1 (task1)
import Task2 (task2)
import Task3 (task3)

main :: IO ()
main = do
  putStrLn "Select a task to run:"
  putStrLn " 1 - Task 1: Hello World"
  putStrLn " 2 - Task 2: Greet User"
  putStrLn " 3 - Task 3: Name and Age"
  putStrLn ""                              
  putStr "Enter choice (1–3): "
  choice <- getLine
  putStrLn ""                          
  runTask choice

runTask :: String -> IO ()
runTask "1" = task1
runTask "2" = task2
runTask "3" = task3
runTask _   = putStrLn "Invalid option. Please choose 1, 2, or 3."
