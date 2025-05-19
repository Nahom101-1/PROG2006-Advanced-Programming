module Task2 (mfact) where
    
mfact :: Integer -> Integer
mfact 0 = 1
mfact n = n * mfact (n - 1)


{- |
  Computes the factorial of a number.

  >>> mfact 0
  1

  >>> mfact 1
  1

  >>> mfact 5
  120

  >>> mfact 7
  5040

  >>> mfact 10
  3628800
-}