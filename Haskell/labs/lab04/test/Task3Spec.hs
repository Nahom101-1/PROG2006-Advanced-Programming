-- test/Task3Spec.hs
module Task3Spec where

import Test.Hspec
import Task3 (countOldest)

spec :: Spec
spec = describe "countOldest" $ do
  it "returns 0 for empty input" $
    countOldest "" `shouldBe` 0

  it "counts oldest students correctly" $ do
    let input = unlines
          [ "Alice Cooper 25"
          , "Alice Boa 23"
          , "Bob Marley 23"
          , "Alice Chains 25"
          , "Charlie Brown 21"
          , "Charlie Chaplin 25"
          , "Eve Wonder 24"
          , "Sandra White 21"
          ]
    countOldest input `shouldBe` 3
