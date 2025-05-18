module Task3 (countOldest) where

-- | Given a string of lines "Name Surname Age", count how many students
-- share the maximum age, in a single traversal.
--
-- >>> let input = unlines
-- ...       [ "Alice Cooper 25"
-- ...       , "Alice Boa 23"
-- ...       , "Bob Marley 23"
-- ...       , "Alice Chains 25"
-- ...       , "Charlie Brown 21"
-- ...       , "Charlie Chaplin 25"
-- ...       , "Eve Wonder 24"
-- ...       , "Sandra White 21"
-- ...       ]
-- >>> countOldest input
-- 3
--
-- >>> countOldest ""
-- 0
countOldest :: String -> Int
countOldest = snd . foldr step (0, 0) . map parseAge . lines
  where
    -- parse the third whitespace-separated token to Int (assumes well-formed input)
    parseAge :: String -> Int
    parseAge line =
      case words line of
        (_:_:ageStr:_) -> read ageStr
        _               -> 0  -- ignore malformed lines

    -- accumulator: (currentMaxAge, countOfMax)
    step :: Int -> (Int, Int) -> (Int, Int)
    step age (maxAge, cnt)
      | age > maxAge = (age, 1)
      | age == maxAge = (maxAge, cnt + 1)
      | otherwise     = (maxAge, cnt)
