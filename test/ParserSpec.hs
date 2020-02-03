{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Data.Either (isLeft, isRight)
import qualified Data.Set as S
import Data.List.NonEmpty (NonEmpty(..))

import Lib
import Parser (parse, esl, aliasDef, symbolDef)
import Types

spec :: Spec
spec = do
  describe "esl module parser" moduleSpec
  describe "alias parser"      aliasSpec
  describe "symbol parser"     symbolSpec

moduleSpec :: Spec
moduleSpec = do
  describe "successfully parses" $ do
    it "an empty module" $ do
      parse esl "-" "" `shouldBe` Right (Module S.empty S.empty)
    it "a list alias" $ do
      let
        input          = "type Bits = Bit*"
        expectedAlias  = Alias "Bits" (Star $ TypeName "Bit")
        expectedModule = Module (S.fromList [expectedAlias]) S.empty
      parse esl "-" input `shouldBe` Right expectedModule
    it "a symbol definition" $ do
      let
        input            = "symbol true: -> Expr"
        expected         = Symbol "true" [] (TypeName "Expr")
        expectedDataType = DataType "Expr" (expected :| [])
        expectedModule   = Module S.empty (S.fromList [expectedDataType])
      parse esl "-" input `shouldBe` Right expectedModule

aliasSpec :: Spec
aliasSpec = do
  describe "successfully parses" $ do
    it "an alias definition" $ do
      let expectedAlias = Alias "Foo" (Product [TypeName "A", TypeName "B"])
      parse aliasDef "-" "type Foo = (A, B)" `shouldBe` Right expectedAlias
  describe "fails at parsing" $ do
    it "the empty string" $ do
      parse aliasDef "-" "" `shouldSatisfy` isLeft

symbolSpec :: Spec
symbolSpec = do
  describe "successfully parses" $ do
    it "a symbol definition" $ do
      let expected = Symbol "foo" [TypeName "A", TypeName "B"] (TypeName "C")
      parse symbolDef "-" "symbol foo: A, B -> C" `shouldBe` Right expected
  describe "fails at parsing" $ do
    it "the empty string" $ do
      parse symbolDef "-" "" `shouldSatisfy` isLeft
