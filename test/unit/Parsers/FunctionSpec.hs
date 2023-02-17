{-# LANGUAGE OverloadedStrings #-}

module Parsers.FunctionSpec (spec) where

import AST
import Programs (func)
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  it "parses a no-op function with no params" $ do
    test func "int aFunc() is return 0 end" `shouldParse` Func WInt (Ident "aFunc" (1, 5)) [] [Return (IntLiter 0 (1, 23)) (1, 16)] Nothing (1, 1)

  it "parses a no-op function with one param" $ do
    test func "int aFunc(char c) is return 0 end" `shouldParse` Func WInt (Ident "aFunc" (1, 5)) [(WChar, Ident "c" (1, 16))] [Return (IntLiter 0 (1, 29)) (1, 22)] Nothing (1, 1)

  it "parses a no-op function with multiple params" $ do
    test func "int aFunc(char c, pair(int, pair) ps) is return 0 end" `shouldParse` Func WInt (Ident "aFunc" (1, 5)) [(WChar, Ident "c" (1, 16)), (WPair WInt (WPair WUnit WUnit), Ident "ps" (1, 35))] [Return (IntLiter 0 (1, 49)) (1, 42)] Nothing (1, 1)

  it "parses a multi statement function with no params" $ do
    test func "int aFunc() is return 7; exit 8; read hello; return 0 end" `shouldParse` Func WInt (Ident "aFunc" (1, 5)) [] [Return (IntLiter 7 (1, 23)) (1, 16), Exit (IntLiter 8 (1, 31)) (1, 26), Read (LIdent (Ident "hello" (1, 39))) (1, 34), Return (IntLiter 0 (1, 53)) (1, 46)] Nothing (1, 1)

  it "fails on function with no is" $ do
    test func `shouldFailOn` "int aFunc() skip end" 

  it "fails on function with no end" $ do
    test func `shouldFailOn` "int aFunc() is skip " 

  it "fails on function with no body" $ do
    test func `shouldFailOn` "int aFunc() is end" 

  it "fails on function with no return type" $ do
    test func `shouldFailOn` "aFunc() is return 0 end" 

  it "fails on function with no bracketed params" $ do
    test func `shouldFailOn` "int aFunc is skip end" 

  it "fails on function not ending with return statement" $ do
    test func `shouldFailOn` "int aFunc() is skip end"