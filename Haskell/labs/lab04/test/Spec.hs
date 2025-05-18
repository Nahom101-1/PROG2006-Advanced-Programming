module Main (main) where

import Test.Hspec

import qualified Task1Spec
import qualified Task2Spec
import qualified Task3Spec 

main :: IO ()
main = hspec $ do
  Task1Spec.spec
  Task2Spec.spec
  Task3Spec.spec
