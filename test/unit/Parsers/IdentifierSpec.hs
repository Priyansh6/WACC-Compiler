{-# LANGUAGE OverloadedStrings #-}

module Parsers.IdentifierSpec (spec) where

import qualified AST
import qualified Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do
  it "multi character" $
    parse Parser.pIdent "" "hello" `shouldParse` AST.Ident "hello"

  it "single char" $
    parse Parser.pIdent "" "a" `shouldParse` AST.Ident "a"

  it "underscore" $
    parse Parser.pIdent "" "_" `shouldParse` AST.Ident "_"

  it "underscore prefixed identifier" $
    parse Parser.pIdent "" "_a" `shouldParse` AST.Ident "_a"

  it "alphanumerical identifier" $
    parse Parser.pIdent "" "aB1_" `shouldParse` AST.Ident "aB1_"

  it "fails invalid identifier" $
    parse Parser.pIdent "" `shouldFailOn` "1"