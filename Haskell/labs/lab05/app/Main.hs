module Main (main) where

import Task1   (mhead1, mhead2, mhead3, mhead4, mhead5, mhead6)
import Task2   (mfact)
import Task3   (fib)
import Task4   (fib2)

main :: IO ()
main = do
  putStrLn "Select a task to run:"
  putStrLn "1 - Task 1: mhead variants"
  putStrLn "2 - Task 2: factorial"
  putStrLn "3 - Task 3: fibonacci recursive"
  putStrLn "4 - Task 4: fibonacci tail recursive"
  choice <- getLine
  runTask choice

runTask :: String -> IO ()
runTask "1" = do
  putStrLn "Enter a list of Ints (e.g. [1,2,3]):"
  inp <- getLine
  let xs = read inp :: [Int]
  putStrLn $ "mhead1: " ++ show (mhead1 xs)
  putStrLn $ "mhead2: " ++ show (mhead2 xs)
  putStrLn $ "mhead3: " ++ show (mhead3 xs)
  putStrLn $ "mhead4: " ++ show (mhead4 xs)
  putStrLn $ "mhead5: " ++ show (mhead5 xs)
  putStrLn $ "mhead6: " ++ show (mhead6 xs)

runTask "2" = do
  putStrLn "Enter n for mfact n:"
  n <- readLn
  putStrLn $ "mfact " ++ show n ++ " = " ++ show (mfact n)

runTask "3" = do
  putStrLn "Enter n for fib n:"
  n <- readLn
  putStrLn $ "fib " ++ show n ++ " = " ++ show (fib n)

runTask "4" = do
  putStrLn "Enter n for fib2 n:"
  n <- readLn
  putStrLn $ "fib2 " ++ show n ++ " = " ++ show (fib2 n)

runTask _   =
  putStrLn "Invalid option. Please choose 1–4."
