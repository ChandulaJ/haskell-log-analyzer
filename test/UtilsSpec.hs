module UtilsSpec where

import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "Utils" $ do
    it "placeholder test" $ do
      True `shouldBe` True
