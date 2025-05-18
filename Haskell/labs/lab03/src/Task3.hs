module Task3 (task3, addNumber, Age(..), addAge) where

import Text.Read (readMaybe)
import Text.Printf (printf)

-- | Generic number addition, works on any ‘Num’ type.
addNumber :: Num a => a -> a -> a
addNumber = (+)

-- | A distinct ‘Age’ type backed by ‘Int’, to enforce domain safety.
newtype Age = Age Int
  deriving (Show, Eq)

-- | Only adds two ‘Age’ values; prevents accidental mixing with raw ‘Int’.
addAge :: Age -> Age -> Age
addAge (Age x) (Age y) = Age (x + y)

-- |  “name and age” task.
task3 :: IO ()
task3 = do
  putStrLn "Hi, what is your name?"
  name <- getLine
  putStrLn "And what is your age?"
  ageStr <- getLine
  case readMaybe ageStr :: Maybe Int of
    Just age ->
      printf "Hello %s, in 10 years you will be %d.\n" name (age + 10)
    Nothing  ->
      putStrLn "That doesn't look like a valid number."
