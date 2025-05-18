module Task4 (mhead1, mhead2, mhead3, mhead4, mhead5) where

-- | mhead1: simple pattern match
--
-- >>> mhead1 [1,2,3]
-- 1
-- >>> mhead1 "abc"
-- 'a'
mhead1 :: [a] -> a
mhead1 (x:_) = x
mhead1 []    = error "empty list"

-- | mhead2: case expression
--
-- >>> mhead2 [True,False]
-- True
mhead2 :: [a] -> a
mhead2 xs = case xs of
  (x:_) -> x
  []    -> error "empty list"

-- | mhead3: foldr trick
--
-- >>> mhead3 [10,20,30]
-- 10
mhead3 :: [a] -> a
mhead3 = foldr (\x _ -> x) (error "empty list")

-- | mhead4: using (!!)
--
-- >>> mhead4 [5,6,7]
-- 5
mhead4 :: [a] -> a
mhead4 xs
  | null xs   = error "empty list"
  | otherwise = xs !! 0

-- | mhead5: guards and length check
--
-- >>> mhead5 "hello"
-- 'h'
mhead5 :: [a] -> a
mhead5 xs
  | length xs > 0 = head xs
  | otherwise     = error "empty list"
