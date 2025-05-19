module Task2Spec (spec) where

import Test.Hspec
import Task2 (mfact) 

-- | unit tests for the factorial function
spec :: Spec
spec = describe "mfact (factorial)" $ do

  it "0! = 1" $
    mfact 0 `shouldBe` 1

  it "1! = 1" $
    mfact 1 `shouldBe` 1

  it "5! = 120" $
    mfact 5 `shouldBe` 120

  it "7! = 5040" $
    mfact 7 `shouldBe` 5040

  it "10! = 3628800" $
    mfact 10 `shouldBe` 3628800
