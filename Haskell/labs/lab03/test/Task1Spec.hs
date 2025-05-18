module Task1Spec where

import Test.Hspec
import Task1 (task1)

--- | Unit test for Task1.
-- This test checks that the task1 function returns unit (IO ()) as expected.
spec :: Spec
spec = describe "task1" $ do
  it "returns unit" $
    task1 `shouldReturn` ()
