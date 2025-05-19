module Main (main) where

import Test.DocTest

-- | Main function to run doctests on the specified modules.
main :: IO ()
main = doctest
  [ "src/Lib.hs"]
