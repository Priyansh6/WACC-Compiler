{-# LANGUAGE OverloadedStrings #-}

module Parsers.IdentifierSpec (spec) where

import AST
import Expressions
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  it "multi character" $
    test pIdent "hello" `shouldParse` Ident "hello"

  it "single char" $
    test pIdent "a" `shouldParse` Ident "a"

  it "underscore" $
    test pIdent "_" `shouldParse` Ident "_"

  it "underscore prefixed identifier" $
    test pIdent "_a" `shouldParse` Ident "_a"

  it "alphanumerical identifier" $
    test pIdent "aB1_" `shouldParse` Ident "aB1_"

  it "fails invalid identifier" $
    test pIdent `shouldFailOn` "1"