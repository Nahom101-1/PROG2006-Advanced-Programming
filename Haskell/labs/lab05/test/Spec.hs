module Main where

import Test.Hspec

import qualified Task1Spec
import qualified Task2Spec
import qualified Task3Spec
import qualified Task4Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Task1Spec.spec
  Task2Spec.spec
  Task3Spec.spec
  Task4Spec.spec
