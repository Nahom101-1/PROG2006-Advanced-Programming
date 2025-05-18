module Task2 (task2) where

-- | Asks the user for their name and greets them.
task2 :: IO ()
task2 = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello " ++ name
