module Task1Spec where

import Test.Hspec
import Task1 (mreverse)

-- | Simple unit tests for mreverse
spec :: Spec
spec = describe "mreverse" $ do

  it "reverses a list of Ints" $
    mreverse [1,2,3,4] `shouldBe` [4,3,2,1]

  it "reverses a String" $
    mreverse "Haskell" `shouldBe` "lleksaH"

  it "returns [] on empty list" $
    mreverse ([] :: [Int]) `shouldBe` []
