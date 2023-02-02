{-# LANGUAGE OverloadedStrings #-}

module Parsers.IdentSpec (spec) where

import qualified AST
import qualified Parser
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  it "parses: hello" $
    parseMaybe Parser.pIdent "hello" `shouldBe` Just (AST.Ident "hello")
  it "parses: a" $
    parseMaybe Parser.pIdent "a" `shouldBe` Just (AST.Ident "a")

  it "parses: _" $
    parseMaybe Parser.pIdent "_" `shouldBe` Just (AST.Ident "_")

  it "parses an underscore prefixed identifier" $
    parseMaybe Parser.pIdent "_a" `shouldBe` Just (AST.Ident "_a")

  it "parses alphanumerical identifier" $
    parseMaybe Parser.pIdent "aB1_" `shouldBe` Just (AST.Ident "aB1_")

  it "can't parse invalid identifier" $
    parseMaybe Parser.pIdent "1" `shouldBe` Nothing