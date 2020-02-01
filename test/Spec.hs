{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Data.Set as S

import Lib
import Parser
import Types

main :: IO ()
main = hspec $ do

  describe "Lib" $ do
    describe "esl2hs correctly parses and produces" $ do
      it "list type aliases" $ do
        esl2hs "" "type Bits = Bit*" `shouldBe` "type Bits = [Bit]\n\n"
      it "tuple type aliases" $ do
        esl2hs "" "type Pair = (A, B)" `shouldBe` "type Pair = (A, B)\n\n"
      it "a symbol" $ do
        esl2hs "" "symbol foo: String, Expr -> Expr"
          `shouldBe` "data Expr\n  = Foo String Expr"

  describe "Util" $ return ()
  describe "Types" $ return ()

  describe "Parser" $ do

    describe "module parser correctly parses" $ do
      it "an empty module" $ do
        parse esl "-" "" `shouldBe` Right (Module S.empty S.empty)
      it "a list alias" $ do
        let
          input          = "type Bits = Bit*"
          expectedAlias  = Alias "Bits" (Star $ TypeName "Bit")
          expectedModule = Module (S.fromList [expectedAlias]) S.empty
        parse esl "-" input `shouldBe` Right expectedModule

    describe "alias parser" $ do
      return ()
    describe " parser" $ do
      return ()
