module CheckEvenSum (checkEvenSum) where


-- | Given a (min, max) pair, check that their sum is even and return half the sum.
checkEvenSum :: (Int, Int) -> Either String Int
checkEvenSum (a, b)
  | even (a + b) = Right ((a + b) `div` 2) -- sum is even
  | otherwise    = Left "Sum of min and max is not even" -- return error message

-- | Check if sum is even
-- >>> checkEvenSum (2,4)
-- Right 3
-- >>> checkEvenSum (2,5)
-- Left "Sum of min and max is not even"
