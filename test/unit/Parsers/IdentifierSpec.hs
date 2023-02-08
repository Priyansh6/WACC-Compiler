{-# LANGUAGE OverloadedStrings #-}

module Parsers.IdentifierSpec (spec) where

import AST
import Expressions (mkIdent)
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  it "multi character" $
    test mkIdent "hello" `shouldParse` Ident "hello" (1, 1)

  it "single char" $
    test mkIdent "a" `shouldParse` Ident "a" (1, 1)

  it "underscore" $
    test mkIdent "_" `shouldParse` Ident "_" (1, 1)

  it "underscore prefixed identifier" $
    test mkIdent "_a" `shouldParse` Ident "_a" (1, 1)

  it "alphanumerical identifier" $
    test mkIdent "aB1_" `shouldParse` Ident "aB1_" (1, 1)

  it "fails invalid identifier" $
    test mkIdent `shouldFailOn` "1"