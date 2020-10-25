{-# LANGUAGE OverloadedStrings #-}

module Positive.LanguageSpec (spec) where

import Positive.Language
import Positive.Prelude
import Test.Hspec

spec :: Spec
spec =
  let test d s r =
        it d $ eval 1 1 <$> parse s `shouldBe` Right r
      testErr d s =
        it d $ eval 1 1 <$> parse s `shouldSatisfy` isLeft
   in describe "Parse and eval" $ do
        testErr "Only Number" "1"
        testErr "Only Pixel" "prr"
        testErr "Only Var" "n"
        testErr "Too much" "1 + 1 + 1"
        test "Pixel" "p + 0.5" 1.5
        test "Var" "n + 0.5" 1.5
        test "Min" "1 - 0.5" 0.5
        test "Mul" "2 * 2" 4.0
        test "Div" "2 / 2" 1.0
        test "Exp" "2 ** 2" 4.0
        test "Parens" "2 + (3 * 2)" 8.0
        test "Sin" "sin 2" 0.9092974268256817
        test "Cos" "cos 2" (-0.4161468365471424)
        test "Neg" "neg 2" (-2.0)
        test "Sqrt" "sqrt 2" 1.4142135623730951
