{-# LANGUAGE OverloadedStrings #-}

module UnitTest.Syntax.ExpressionSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec

import AST
import Syntax.Expressions (expr, arrayElem, ident)
import UnitTest.Syntax.Test

spec :: Spec
spec = do
  it "int multiplication" $
    test expr "1 * 2" `shouldParse` (IntLiter 1 (1, 1) :*: IntLiter 2 (1, 5)) (1, 3)

  it "int division" $
    test expr "1 / 2" `shouldParse` (IntLiter 1 (1, 1) :/: IntLiter 2 (1, 5)) (1, 3)

  it "int modulo" $
    test expr "1 % 2" `shouldParse` (IntLiter 1 (1, 1) :%: IntLiter 2 (1, 5)) (1, 3)

  it "int addition" $
    test expr "1 + 2" `shouldParse` (IntLiter 1 (1, 1) :+: IntLiter 2 (1, 5)) (1, 3)

  it "int subtraction" $
    test expr "1 - 2" `shouldParse` (IntLiter 1 (1, 1) :-: IntLiter 2 (1, 5)) (1, 3)

  it "binary op without whitespace" $
    test expr "1-2" `shouldParse` (IntLiter 1 (1, 1) :-: IntLiter 2 (1, 3)) (1, 2)

  it "binary ops with higher precedence than another" $
    test expr "1 + 2 * 3" `shouldParse` (IntLiter 1 (1, 1) :+: (IntLiter 2 (1, 5) :*: IntLiter 3 (1, 9)) (1, 7)) (1, 3)

  it "binary ops of same precedence with parentheses" $
    test expr "(1 * 2) * 3" `shouldParse` ((IntLiter 1 (1, 2) :*: IntLiter 2 (1, 6)) (1, 4) :*: IntLiter 3 (1, 11)) (1, 9)

  it "binary ops of different precedence with parentheses" $
    test expr "(1 + 2) * 3" `shouldParse` ((IntLiter 1 (1, 2) :+: IntLiter 2 (1, 6)) (1, 4) :*: IntLiter 3 (1, 11)) (1, 9)

  it "boolean AND" $
    test expr "true && false" `shouldParse` (BoolLiter True (1, 1) :&&: BoolLiter False (1, 9)) (1, 6)

  it "boolean OR" $
    test expr "true || false" `shouldParse` (BoolLiter True (1, 1) :||: BoolLiter False (1, 9)) (1, 6)

  it "boolean equality" $
    test expr "true == false" `shouldParse` (BoolLiter True (1, 1) :==: BoolLiter False (1, 9)) (1, 6)

  it "int equality" $
    test expr "1 == 2" `shouldParse` (IntLiter 1 (1, 1) :==: IntLiter 2 (1, 6)) (1, 3)

  it "character equality" $
    test expr "'a' == 'b'" `shouldParse` (CharLiter 'a' (1, 1) :==: CharLiter 'b' (1, 8)) (1, 5)

  it "string equality" $
    test expr "\"hello\" == \"world\"" `shouldParse` (StrLiter "hello" (1, 1) :==: StrLiter "world" (1, 12)) (1, 9)

  it "negation of a parenthesised expression" $
    test expr "-(1 + 2)" `shouldParse` Neg ((IntLiter 1 (1, 3) :+: IntLiter 2 (1, 7)) (1, 5)) (1, 1)

  it "multiple subexpressions involving idents" $
    test expr "-(1 * x) + (y / 2)" `shouldParse` (Neg 
      ((IntLiter 1 (1, 3) :*: IdentExpr (Ident "x" (1, 7)) (1, 7)) (1, 5)) (1, 1)
      :+: 
      (IdentExpr (Ident "y" (1, 13)) (1, 13) :/: IntLiter 2 (1, 17)) (1, 15)) (1, 10)

  it "expression with len unop" $
    test expr "len 5" `shouldParse` Len (IntLiter 5 (1, 5)) (1, 1)

  it "expression with ord unop" $
    test expr "ord 5" `shouldParse` Ord (IntLiter 5 (1, 5)) (1, 1)

  it "expression with chr unop" $
    test expr "chr 5" `shouldParse` Chr (IntLiter 5 (1, 5)) (1, 1)

  it "nested unaries" $
    test expr "chr (ord 5)" `shouldParse` Chr (Ord (IntLiter 5 (1, 10)) (1, 6)) (1, 1)

  it "len unop applied to no arguments" $
    test expr `shouldFailOn` "len"

  it "chr unop applied to a paranthesised expression" $
    test expr "chr(5 * (6 + 7))" `shouldParse` Chr ((IntLiter 5 (1, 5) :*: (IntLiter 6 (1, 10) :+: IntLiter 7 (1, 14)) (1, 12)) (1, 7)) (1, 1)

  it "array elem with dimension of 1" $
    test arrayElem "arr[5]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IntLiter 5 (1, 5)] (1, 1)

  it "identifier _" $
    test ident "_" `shouldParse` Ident "_" (1, 1)

  it "single char identifier" $
    test ident "a" `shouldParse` Ident "a" (1, 1)

  it "identifier multichar identifier" $
    test ident "abc" `shouldParse` Ident "abc" (1, 1)