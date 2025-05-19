module Task1Spec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Task1

-- | unit tests for mhead variants
spec :: Spec
spec = describe "mhead variants" $ do

  it "mhead1 matches head" $
    mhead1 [1,2,3] `shouldBe` (1 :: Int)

  it "mhead2 matches head" $
    mhead2 "abc" `shouldBe` 'a'

  it "mhead3 matches head" $
    mhead3 [True,False] `shouldBe` True

  it "mhead4 matches head" $
    mhead4 ["x", "y"] `shouldBe` "x"

  it "mhead5 matches head" $
    mhead5 [10.5,20.5] `shouldBe` (10.5 :: Double)

  it "mhead6 matches head" $
    mhead6 [Just 42, Nothing] `shouldBe` Just 42

  describe "error on empty list" $ do
    it "mhead1 throws on []" $
      evaluate (mhead1 [] :: Int) `shouldThrow` anyErrorCall

    it "mhead2 throws on []" $
      evaluate (mhead2 [] :: Int) `shouldThrow` anyErrorCall

    it "mhead3 throws on []" $
      evaluate (mhead3 [] :: Int) `shouldThrow` anyErrorCall

    it "mhead4 throws on []" $
      evaluate (mhead4 [] :: Int) `shouldThrow` anyErrorCall

    it "mhead5 throws on []" $
      evaluate (mhead5 [] :: Int) `shouldThrow` anyErrorCall

    it "mhead6 throws on []" $
      evaluate (mhead6 [] :: Int) `shouldThrow` anyErrorCall
