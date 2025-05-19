module Main (main) where

import Lib (countScore)

-- | Read the input data from stdin and print the total score of the game.
--
main :: IO ()
main = getContents >>= putStrLn . ("The total score is: " ++) . show . countScore

-- to see the result
-- stack exec lab07-exe < data.txt