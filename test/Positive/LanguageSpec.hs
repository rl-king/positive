{-# LANGUAGE OverloadedStrings #-}

module Positive.LanguageSpec (spec) where

import Positive.Language
import Positive.Prelude
import Test.Hspec

spec :: Spec
spec =
  let test d s r =
        it d $
          eval 1 1 <$> parse s `shouldBe` Right r
   in describe "Parse and eval" $ do
        test "Number" "1" 1.0
        test "Number neg" "-1" (-1.0)
        test "Pixel" "p" 1.0
        test "Var" "n" 1.0
        test "Plus" "1 + 1" 2.0
        test "Min" "1 - 1" 0.0
        test "Mul" "2 * 2" 4.0
        test "Exp" "2 ** 2" 4.0
