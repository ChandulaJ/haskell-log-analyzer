module ParserSpec where

import Test.Hspec
import Parser

spec :: Spec
spec = do
  describe "Parser" $ do
    it "placeholder test" $ do
      True `shouldBe` True
