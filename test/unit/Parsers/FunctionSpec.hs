{-# LANGUAGE OverloadedStrings #-}

module Parsers.FunctionSpec (spec) where

import AST
import Programs (pFunc)
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  it "parses a no-op function with no params" $ do
    test pFunc "int aFunc() is skip end" `shouldParse` Func WInt (Ident "aFunc") [] [Skip]

  it "parses a no-op function with one param" $ do
    test pFunc "int aFunc(char c) is skip end" `shouldParse` Func WInt (Ident "aFunc") [(WChar, Ident "c")] [Skip]

  it "parses a no-op function with multiple params" $ do
    test pFunc "int aFunc(char c, pair(int, pair) ps) is skip end" `shouldParse` Func WInt (Ident "aFunc") [(WChar, Ident "c"), (WPair WInt WUnit, Ident "ps")] [Skip]

  it "parses a multi statement function with no params" $ do
    test pFunc "int aFunc() is return 7; exit 8; read hello end" `shouldParse` Func WInt (Ident "aFunc") [] [Return (IntLiter 7), Exit (IntLiter 8), Read (LIdent (Ident "hello"))]

  it "fails on function with no is" $ do
    test pFunc `shouldFailOn` "int aFunc() skip end" 

  it "fails on function with no end" $ do
    test pFunc `shouldFailOn` "int aFunc() is skip " 

  it "fails on function with no body" $ do
    test pFunc `shouldFailOn` "int aFunc() is end" 

  it "fails on function with no return type" $ do
    test pFunc `shouldFailOn` "aFunc() is skip end" 

  it "fails on function with no bracketed params" $ do
    test pFunc `shouldFailOn` "int aFunc is skip end" 