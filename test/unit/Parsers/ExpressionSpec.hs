{-# LANGUAGE OverloadedStrings #-}

module Parsers.ExpressionSpec (spec) where

import AST
import Expressions
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec
import Expressions (pExpr)

spec :: Spec
spec = do
  it "int multiplication" $
    test pExpr "1 * 2" `shouldParse` (IntLiter 1 (1, 1) :*: IntLiter 2 (1, 5)) (1, 3)

  it "int division" $
    test pExpr "1 / 2" `shouldParse` (IntLiter 1 (1, 1) :/: IntLiter 2 (1, 5)) (1, 3)

  it "int modulo" $
    test pExpr "1 % 2" `shouldParse` (IntLiter 1 (1, 1) :%: IntLiter 2 (1, 5)) (1, 3)

  it "int addition" $
    test pExpr "1 + 2" `shouldParse` (IntLiter 1 (1, 1) :+: IntLiter 2 (1, 5)) (1, 3)

  it "int subtraction" $
    test pExpr "1 - 2" `shouldParse` (IntLiter 1 (1, 1) :-: IntLiter 2 (1, 5)) (1, 3)

  it "binary op without whitespace" $
    test pExpr "1-2" `shouldParse` (IntLiter 1 (1, 1) :-: IntLiter 2 (1, 3)) (1, 2)

  it "binary ops with higher precedence than another" $
    test pExpr "1 + 2 * 3" `shouldParse` (IntLiter 1 (1, 1) :+: (IntLiter 2 (1, 5) :*: IntLiter 3 (1, 9)) (1, 7)) (1, 3)

  it "binary ops of same precedence with parentheses" $
    test pExpr "(1 * 2) * 3" `shouldParse` ((IntLiter 1 (1, 2) :*: IntLiter 2 (1, 6)) (1, 4) :*: IntLiter 3 (1, 11)) (1, 9)

  it "binary ops of different precedence with parentheses" $
    test pExpr "(1 + 2) * 3" `shouldParse` ((IntLiter 1 (1, 2) :+: IntLiter 2 (1, 6)) (1, 4) :*: IntLiter 3 (1, 11)) (1, 9)

  it "boolean AND" $
    test pExpr "true && false" `shouldParse` (BoolLiter True (1, 1) :&&: BoolLiter False (1, 9)) (1, 6)

  it "boolean OR" $
    test pExpr "true || false" `shouldParse` (BoolLiter True (1, 1) :||: BoolLiter False (1, 9)) (1, 6)

  it "boolean equality" $
    test pExpr "true == false" `shouldParse` (BoolLiter True (1, 1) :==: BoolLiter False (1, 9)) (1, 6)

  it "int equality" $
    test pExpr "1 == 2" `shouldParse` (IntLiter 1 (1, 1) :==: IntLiter 2 (1, 6)) (1, 3)

  it "character equality" $
    test pExpr "'a' == 'b'" `shouldParse` (CharLiter 'a' (1, 1) :==: CharLiter 'b' (1, 8)) (1, 5)

  it "string equality" $
    test pExpr "\"hello\" == \"world\"" `shouldParse` (StrLiter "hello" (1, 1) :==: StrLiter "world" (1, 12)) (1, 9)

  it "negation of a parenthesised expression" $
    test pExpr "-(1 + 2)" `shouldParse` Neg ((IntLiter 1 (1, 3) :+: IntLiter 2 (1, 7)) (1, 5)) (1, 1)

  it "multiple subexpressions involving idents" $
    test pExpr "-(1 * x) + (y / 2)" `shouldParse` (Neg 
      ((IntLiter 1 (1, 3) :*: IdentExpr (Ident "x" (1, 7)) (1, 7)) (1, 5)) (1, 1)
      :+: 
      (IdentExpr (Ident "y" (1, 13)) (1, 13) :/: IntLiter 2 (1, 17)) (1, 15)) (1, 10)

  it "expression with len unop" $
    test pExpr "len 5" `shouldParse` Len (IntLiter 5 (1, 5)) (1, 1)

  it "expression with ord unop" $
    test pExpr "ord 5" `shouldParse` Ord (IntLiter 5 (1, 5)) (1, 1)

  it "expression with chr unop" $
    test pExpr "chr 5" `shouldParse` Chr (IntLiter 5 (1, 5)) (1, 1)

  it "nested unaries" $
    test pExpr "chr (ord 5)" `shouldParse` Chr (Ord (IntLiter 5 (1, 10)) (1, 6)) (1, 1)

  it "len unop applied to no arguments" $
    test pExpr `shouldFailOn` "len"

  it "chr unop applied to a paranthesised expression" $
    test pExpr "chr(5 * (6 + 7))" `shouldParse` Chr ((IntLiter 5 (1, 5) :*: (IntLiter 6 (1, 10) :+: IntLiter 7 (1, 14)) (1, 12)) (1, 7)) (1, 1)

  it "array elem with dimension of 1" $
    test mkArrayElem "arr[5]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IntLiter 5 (1, 5)] (1, 1)

  it "identifier _" $
    test mkIdent "_" `shouldParse` Ident "_" (1, 1)

  it "single char identifier" $
    test mkIdent "a" `shouldParse` Ident "a" (1, 1)

  it "identifier multichar identifier" $
    test mkIdent "abc" `shouldParse` Ident "abc" (1, 1)