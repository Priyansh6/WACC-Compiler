{-# LANGUAGE OverloadedStrings #-}

module Parsers.LiteralsSpec (spec) where

import AST
import Expressions
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  describe "Boolean" $ do
    it "true" $ do
      test pBool "true" `shouldParse` BoolLiter True

    it "false" $
      test pBool "false" `shouldParse` BoolLiter False

    xit "fails bool appended with anything" $
      test pBool `shouldFailOn` "truetrue"

    it "fails non-bools" $
      test pBool `shouldFailOn` "e"

    it "trailing whitespace" $
      test pBool "true " `shouldParse` BoolLiter True

  describe "Integer" $ do
    it "unsigned int" $
      test pInt "123" `shouldParse` IntLiter 123

    it "positive int" $
      test pInt "+123" `shouldParse` IntLiter 123

    it "negative int" $
      test pInt "-123" `shouldParse` IntLiter (-123)

    it "fails on just sign (+, -)" $
      test pInt `shouldFailOn` "+"

    it "fails on nothing" $
      test pInt `shouldFailOn` ""

  describe "Character" $ do
    it "single char" $
      test pChar "'a'" `shouldParse` CharLiter 'a'

    it "fails on empty char" $
      test pChar `shouldFailOn` "''"

    it "fails on multiple chars" $
      test pChar `shouldFailOn` "'aa'"

    it "fails on nothing" $
      test pChar `shouldFailOn` ""

  describe "String" $ do
    it "multi char string" $
      test pString "\"aa\"" `shouldParse` StrLiter "aa"

    it "empty string" $
      test pString "\"\"" `shouldParse` StrLiter ""

    xit "fails on too many quote marks" $
      test pString `shouldFailOn` "\"a\"\""

    it "fails on nothing" $
      test pString `shouldFailOn` ""

  describe "Pair" $ do
    it "pair literal" $
      test pPairLit "null" `shouldParse` PairLiter

    it "fails on nothing" $
      test pPairLit `shouldFailOn` ""