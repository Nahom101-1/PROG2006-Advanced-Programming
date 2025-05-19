module Task4Spec (spec) where

import Test.Hspec
import Task4 (fib2) -- Correct module name

spec :: Spec
spec = describe "fib2 (infinite list via zipWith)" $ do

  it "fib2 0 = 0" $
    fib2 0 `shouldBe` 0

  it "fib2 1 = 1" $
    fib2 1 `shouldBe` 1

  it "fib2 2 = 1" $
    fib2 2 `shouldBe` 1

  it "fib2 5 = 5" $
    fib2 5 `shouldBe` 5

  it "fib2 10 = 55" $
    fib2 10 `shouldBe` 55

  it "fib2 15 = 610" $
    fib2 15 `shouldBe` 610
