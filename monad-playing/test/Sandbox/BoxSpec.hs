module Sandbox.BoxSpec (spec) where

import Test.Hspec


spec :: Spec
spec = do
  describe "monads" $ do
    it "have bind operator" $ do
      "toto" `shouldBe` "toto"

    it "has bind operator" $ do
      let
        add2 :: Integer -> Maybe Integer
        add2 a = Just (a + 2)
      (Just 4 >>= add2) `shouldBe` Just 6
