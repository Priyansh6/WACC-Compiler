{-# LANGUAGE OverloadedStrings #-}

module Parsers.IdentifierSpec (spec) where

import AST
import Expressions (ident)
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  it "multi character" $
    test ident "hello" `shouldParse` Ident "hello" (1, 1)

  it "single char" $
    test ident "a" `shouldParse` Ident "a" (1, 1)

  it "underscore" $
    test ident "_" `shouldParse` Ident "_" (1, 1)

  it "underscore prefixed identifier" $
    test ident "_a" `shouldParse` Ident "_a" (1, 1)

  it "alphanumerical identifier" $
    test ident "aB1_" `shouldParse` Ident "aB1_" (1, 1)

  it "fails invalid identifier" $
    test ident `shouldFailOn` "1"