module Task3Spec (spec) where

import Test.Hspec
import Task3 (fib)

-- | Unit tests for the recursive Fibonacci function
spec :: Spec
spec = describe "fib recursive" $ do
  it "fib 0 = 0" $
    fib 0 `shouldBe` 0

  it "fib 1 = 1" $
    fib 1 `shouldBe` 1

  it "fib 5 = 5" $
    fib 5 `shouldBe` 5

  it "fib 7 = 13" $
    fib 7 `shouldBe` 13