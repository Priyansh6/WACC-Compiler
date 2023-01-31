{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec

import qualified Parser 
import qualified AST 

import Text.Megaparsec

spec :: Spec
spec = do
  it "parses true to a BoolLiter True" $ do
    parseMaybe Parser.pBool "true" `shouldBe` (Just (AST.BoolLiter True))

  it "parses false to a BoolLiter False" $
    parseMaybe Parser.pBool "false" `shouldBe` (Just (AST.BoolLiter False))

  it "can't parse strings prefixed with a bool to BoolLiter" $
    parseMaybe Parser.pBool "truetrue" `shouldBe` Nothing

  it "can't parse non-bools to BoolLiter" $
    parseMaybe Parser.pBool "eobtih21" `shouldBe` Nothing

  it "parses bools with trailing whitespace" $
    parseMaybe Parser.pBool "true " `shouldBe` (Just (AST.BoolLiter True))