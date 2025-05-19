module Main (main) where

import Test.DocTest

-- | Main function to run doctests on the specified modules.
main :: IO ()
main = doctest
  [ "src/Count.hs"
  , "src/CheckEvenSum.hs"
  , "src/UniqueMinMax.hs"
  , "src/CountMagicNumber.hs"
  , "src/Decoder.hs"
  ]
