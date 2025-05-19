-- test/Spec.hs
module Main (main) where

import Test.Hspec

import qualified Task1Spec
import qualified Task2Spec
import qualified Task3Spec
import qualified Task4Spec

main :: IO ()
main = hspec $ do
  Task1Spec.spec
  Task2Spec.spec
  Task3Spec.spec
  Task4Spec.spec
