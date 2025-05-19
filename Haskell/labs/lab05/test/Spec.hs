module Main (main) where

import Test.Hspec
import Task1Spec      -- keep only this
-- import Task2Spec  -- comment out
-- import Task3Spec  -- comment out
-- import Task4Spec  -- comment out

main :: IO ()
main = hspec $ do
  Task1Spec.spec
--  Task2Spec.spec
--  Task3Spec.spec
--  Task4Spec.spec
