module Task3 (fib) where

-- | Naïve recursive Fibonacci.
--
-- >>> fib 0
-- 0
--
-- >>> fib 1
-- 1
--
-- >>> fib 5
-- 5
--
-- >>> fib 7
-- 13
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n < 0     = error "Negative number not allowed"
  | otherwise = fib (n - 1) + fib (n - 2) 
