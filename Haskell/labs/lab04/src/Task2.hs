module Task2 (mulTable) where

-- | Print an n×n multiplication table, padded to width 3.
mulTable :: Int -> IO ()
mulTable n = putStrLn $ unlines [row i | i <- [1..n]]
  where
    row i = concat [pad (i * j) | j <- [1..n]]  -- for each row, calculate the product
    pad num = let s = show num in replicate (3 - length s) ' ' ++ s -- pad the number to width 3
