{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Test.Hspec
import qualified Data.Set as S

import Lib (esl2hs)

spec :: Spec
spec = do
  describe "esl2hs correctly parses and produces" $ do

    it "an empty module" $ do
      esl2hs "-" "" `shouldBe` ""

    it "a list alias" $ do
      esl2hs "-" "type Bits = Bit*" `shouldBe` "type Bits = [Bit]"

    it "a tuple alias" $ do
      esl2hs "-" "type Pair = (A, B)" `shouldBe` "type Pair = (A, B)"

    it "multiple aliases" $ do
      let
        first  = "type Bits = Bit*"
        second = "type Pair = (A, B)"
      esl2hs "-" (first ++ "\n" ++ second)
        `shouldBe` "type Bits = [Bit]\ntype Pair = (A, B)"

    it "a symbol" $ do
      esl2hs "-" "symbol foo: String, Expr -> Expr"
        `shouldBe` "data Expr\n  = Foo String Expr"

    it "multiple symbols" $ do
      let
        first  = "symbol true: -> Expr"
        second = "symbol false: -> Expr"
        third  = "symbol and: Expr, Expr -> Expr"
      esl2hs "-" (unlines [first, second, third])
        `shouldBe` "data Expr\n  = True\n  | False\n  | And Expr Expr"

    it "aliases and symbols together" $ do
      let
        first  = "type Bits = Bit*"
        symbol = "symbol false: -> Expr"
      esl2hs "-" (unlines [first, symbol])
        `shouldBe` "type Bits = [Bit]\n\ndata Expr\n  = False"
