module Task4Spec where

import Test.Hspec
import Control.Exception (evaluate)
import Task4

-- | Unit tests for Task4.
-- This module contains tests for the mhead1, mhead2, mhead3, mhead4, and mhead5 functions.
spec :: Spec
spec = describe "mhead variants (simple tests)" $ do

  it "mhead1 returns first element" $
    mhead1 [1,2,3] `shouldBe` (1 :: Int)

  it "mhead2 returns first element" $
    mhead2 "abc" `shouldBe` 'a'

  it "mhead3 returns first element" $
    mhead3 [True, False] `shouldBe` True

  it "mhead4 returns first element" $
    mhead4 ["x","y"] `shouldBe` "x"

  it "mhead5 returns first element" $
    mhead5 [10.5,20.5] `shouldBe` (10.5 :: Double)

  describe "empty list errors" $ do
    it "mhead1 throws on []" $
      evaluate (mhead1 []) `shouldThrow` anyErrorCall

    it "mhead2 throws on []" $
      evaluate (mhead2 []) `shouldThrow` anyErrorCall

    it "mhead3 throws on []" $
      evaluate (mhead3 []) `shouldThrow` anyErrorCall

    it "mhead4 throws on []" $
      evaluate (mhead4 []) `shouldThrow` anyErrorCall

    it "mhead5 throws on []" $
      evaluate (mhead5 []) `shouldThrow` anyErrorCall
