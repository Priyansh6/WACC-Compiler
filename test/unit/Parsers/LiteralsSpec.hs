{-# LANGUAGE OverloadedStrings #-}

module Parsers.LiteralsSpec (spec) where

import qualified AST
import qualified Expressions
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "Boolean" $ do
    it "true" $ do
      parse Expressions.pBool "" "true" `shouldParse` AST.BoolLiter True

    it "false" $
      parse Expressions.pBool "" "false" `shouldParse` AST.BoolLiter False

    it "fails bool appended with anything" $
      parse Expressions.pBool "" `shouldFailOn` "truetrue"

    it "fails non-bools" $
      parse Expressions.pBool "" `shouldFailOn` "e"

    it "trailing whitespace" $
      parse Expressions.pBool "" "true " `shouldParse` AST.BoolLiter True

  describe "Integer" $ do
    it "unsigned int" $
      parse Expressions.pInt "" "123" `shouldParse` AST.IntLiter 123

    it "positive int" $
      parse Expressions.pInt "" "+123" `shouldParse` AST.IntLiter 123

    it "negative int" $
      parse Expressions.pInt "" "-123" `shouldParse` AST.IntLiter (-123)

    it "fails on just sign (+, -)" $
      parse Expressions.pInt "" `shouldFailOn` "+"

    it "fails on nothing" $
      parse Expressions.pInt "" `shouldFailOn` ""

  describe "Character" $ do
    it "single char" $
      parse Expressions.pChar "" "'a'" `shouldParse` AST.CharLiter 'a'

    it "fails on empty char" $
      parse Expressions.pChar "" `shouldFailOn` "''"

    it "fails on multiple chars" $
      parse Expressions.pChar "" `shouldFailOn` "'aa'"

    it "fails on nothing" $
      parse Expressions.pChar "" `shouldFailOn` ""

  describe "String" $ do
    it "multi char string" $
      parse Expressions.pString "" "\"aa\"" `shouldParse` AST.StrLiter "aa"

    it "empty string" $
      parse Expressions.pString "" "\"\"" `shouldParse` AST.StrLiter ""

    it "fails on too many quote marks" $
      parse Expressions.pString "" `shouldFailOn` "\"a\"\""

    it "fails on nothing" $
      parse Expressions.pString "" `shouldFailOn` ""

  describe "Pair" $ do
    it "pair literal" $
      parse Expressions.pPairLit "" "null" `shouldParse` AST.PairLiter

    it "fails on nothing" $
      parse Expressions.pPairLit "" `shouldFailOn` ""