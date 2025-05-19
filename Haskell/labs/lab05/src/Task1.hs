module Task1 (mhead1, mhead2, mhead3, mhead4, mhead5, mhead6) where

-- 1. Using function argument pattern matching
--
-- >>> mhead1 [1,2,3]
-- 1
-- >>> mhead1 "abc"
-- 'a'
mhead1 :: [a] -> a
mhead1 (x:_) = x
mhead1 []    = error "empty list"

-- 2. Using function guards
--
-- >>> mhead2 [True,False]
-- True
-- >>> mhead2 "xyz"
-- 'x'
mhead2 :: [a] -> a
mhead2 xs
  | null xs   = error "empty list"
  | otherwise = case xs of
      (x:_) -> x
      []    -> error "empty list"

-- 3. Using a single line with if ... else ... expressions
--
-- >>> mhead3 [42,99]
-- 42
-- >>> mhead3 "ok"
-- 'o'
mhead3 :: [a] -> a
mhead3 xs = if null xs then error "empty list" else case xs of
    (x:_) -> x
    []    -> error "empty list"

-- 4. Using let .. in ..
--
-- >>> mhead4 [7,8,9]
-- 7
-- >>> mhead4 "hi"
-- 'h'
mhead4 :: [a] -> a
mhead4 xs =
    let first = if null xs then error "empty list" else case xs of
          (x:_) -> x
          []    -> error "empty list"
    in first

-- 5. Using where expression
--
-- >>> mhead5 [3.14,2.71]
-- 3.14
-- >>> mhead5 "go"
-- 'g'
mhead5 :: [a] -> a
mhead5 xs
  | null xs   = error "empty list"
  | otherwise = firstElement
  where
    firstElement = case xs of
      (x:_) -> x
      []    -> error "empty list"

-- 6. Using case .. of .. expression
--
-- >>> mhead6 [0,1]
-- 0
-- >>> mhead6 "!!"
-- '!'
mhead6 :: [a] -> a
mhead6 xs = case xs of
    (x:_) -> x
    []    -> error "empty list"
