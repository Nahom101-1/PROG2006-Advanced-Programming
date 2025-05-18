module Task1 (mreverse) where

-- | mreverse is my own implementation of list reversal
--
-- >>> mreverse "Hello"
-- "olleH"
--
-- >>> mreverse [1,2,3]
-- [3,2,1]
mreverse :: [a] -> [a]
mreverse = go []
  where
    go :: [a] -> [a] -> [a]
    go acc []     = acc          -- when input is empty, return accumulated result
    go acc (x:xs) = go (x : acc) xs  -- push head onto acc, recurse on tail