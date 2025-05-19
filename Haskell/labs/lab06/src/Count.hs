module Count (count) where

-- | Count how many times a value appears in a list.
count :: Eq a => a -> [a] -> Int --
count x = length . filter (== x) -- uses filter to create a list of elements equal to x, then counts them with length

-- | Count how many times a value appears in a list.
-- >>> count 3 [1,2,3,3,4]
-- 2
-- >>> count 'a' "banana"
-- 3
-- >>> count True [True, False, True]
-- 2
-- >>> count 0 []
-- 0