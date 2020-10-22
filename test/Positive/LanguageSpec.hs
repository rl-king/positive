{-# LANGUAGE OverloadedStrings #-}

module Positive.LanguageSpec (spec) where

import Positive.Language
import Positive.Prelude
import Test.Hspec

spec :: Spec
spec =
  describe "Parse and eval" $ do
    it "Number" $
      eval 1 1 <$> parse "1" `shouldBe` Right 1.0
