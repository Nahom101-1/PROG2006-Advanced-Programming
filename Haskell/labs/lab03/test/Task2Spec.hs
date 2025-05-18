module Task2Spec where

import Test.Hspec
import Task2 (task2)

-- | Unit test for Task2.
-- This test checks that the task2 function returns unit (IO ()) as expected.
spec :: Spec
spec = describe "task2" $ do
  it "returns unit" $
    task2 `shouldReturn` ()
