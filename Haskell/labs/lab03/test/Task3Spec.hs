module Task3Spec where

import Test.Hspec
import Task3 (task3, addNumber, Age(..), addAge)

-- | Unit test for Task3.
-- This test checks the functionality of the addNumber and addAge functions,
spec :: Spec
spec = describe "Task3 utilities and task3" $ do

  describe "addNumber" $ do
    it "adds two ints" $
      addNumber (5 :: Int) 7 `shouldBe` 12

    it "adds two floats" $
      addNumber (1.5 :: Float) 2.5 `shouldBe` 4.0

  describe "addAge" $ do
    it "adds two Age values" $
      addAge (Age 10) (Age 15) `shouldBe` Age 25

    it "maintains type safety (compile-time only)" $ do
      addAge (Age 1) (Age 2) `shouldBe` Age 3

  describe "task3 interactive" $ do
    it "returns unit" $
      task3 `shouldReturn` ()
