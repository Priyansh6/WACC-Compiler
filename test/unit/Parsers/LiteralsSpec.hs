{-# LANGUAGE OverloadedStrings #-}

module Parsers.LiteralsSpec (spec) where

import qualified AST
import qualified Expressions
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "Boolean" $ do
    it "parses true" $ do
      parseMaybe Expressions.pBool "true" `shouldBe` Just (AST.BoolLiter True)

    it "parses false" $
      parseMaybe Expressions.pBool "false" `shouldBe` Just (AST.BoolLiter False)

    it "can't parse strings prefixed with a bool" $
      parseMaybe Expressions.pBool "truetrue" `shouldBe` Nothing

    it "can't parse non-bools" $
      parseMaybe Expressions.pBool "e" `shouldBe` Nothing

    it "parses bools with trailing whitespace" $
      parseMaybe Expressions.pBool "true " `shouldBe` Just (AST.BoolLiter True)

  describe "Integer" $ do
    it "parses unsigned integer literal" $
      parseMaybe Expressions.pInt "123" `shouldBe` Just (AST.IntLiter 123)

    it "parses positive integer literal" $
      parseMaybe Expressions.pInt "+123" `shouldBe` Just (AST.IntLiter 123)

    it "parses negative integer literal" $
      parseMaybe Expressions.pInt "-123" `shouldBe` Just (AST.IntLiter (-123))

    it "can't parse only a sign" $
      parseMaybe Expressions.pInt "+" `shouldBe` Nothing

    it "can't parse nothing" $
      parseMaybe Expressions.pInt "" `shouldBe` Nothing

  describe "Character" $ do
    it "parses 'a'" $
      parseMaybe Expressions.pChar "'a'" `shouldBe` Just (AST.CharLiter 'a')

    it "can't parse empty character" $
      parseMaybe Expressions.pChar "''" `shouldBe` Nothing

    it "can't parse multiple characters" $
      parseMaybe Expressions.pChar "'aa'" `shouldBe` Nothing

    it "can't parse nothing" $
      parseMaybe Expressions.pChar "" `shouldBe` Nothing

  describe "String" $ do
    it "parses \"aa\"" $
      parseMaybe Expressions.pString "\"aa\"" `shouldBe` Just (AST.StrLiter "aa")

    it "parses empty string" $
      parseMaybe Expressions.pString "\"\"" `shouldBe` Just (AST.StrLiter "")

    it "can't parse incorrect string notation" $
      parseMaybe Expressions.pString "\"a\"\"" `shouldBe` Nothing

    it "can't parse nothing" $
      parseMaybe Expressions.pString "" `shouldBe` Nothing

  describe "Pair" $ do
    it "parses a pair literal" $
      parseMaybe Expressions.pPairLit "null" `shouldBe` Just AST.PairLiter

    it "can't the empty string" $
      parseMaybe Expressions.pPairLit "" `shouldBe` Nothing