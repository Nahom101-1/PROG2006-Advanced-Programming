module Task2Spec where

import Test.Hspec
import Task2 (mulTable)

spec :: Spec
spec = describe "mulTable" $ do

  it "returns unit even when printing" $
    mulTable 3 `shouldReturn` ()
