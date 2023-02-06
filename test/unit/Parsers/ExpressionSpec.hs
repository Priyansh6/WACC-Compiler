{-# LANGUAGE OverloadedStrings #-}

module Parsers.ExpressionSpec (spec) where

import AST
import Expressions
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec
import Expressions (pIdent)
import AST (Expr(CharLiter))

spec :: Spec
spec = do
  it "int multiplication" $
    test pExpr "1 * 2" `shouldParse` (IntLiter 1 :*: IntLiter 2)

  it "int division" $
    test pExpr "1 / 2" `shouldParse` (IntLiter 1 :/: IntLiter 2)

  it "int modulo" $
    test pExpr "1 % 2" `shouldParse` (IntLiter 1 :%: IntLiter 2)

  it "int addition" $
    test pExpr "1 + 2" `shouldParse` (IntLiter 1 :+: IntLiter 2)

  it "int subtraction" $
    test pExpr "1 - 2" `shouldParse` (IntLiter 1 :-: IntLiter 2)

  it "binary op without whitespace" $
    test pExpr "1-2" `shouldParse` (IntLiter 1 :-: IntLiter 2)

  it "binary ops with higher precedence than another" $
    test pExpr "1 + 2 * 3" `shouldParse` (IntLiter 1 :+: (IntLiter 2 :*: IntLiter 3))

  it "binary ops of same precedence with parentheses" $
    test pExpr "(1 * 2) * 3" `shouldParse` ((IntLiter 1 :*: IntLiter 2) :*: IntLiter 3)

  it "binary ops of different precedence with parentheses" $
    test pExpr "(1 + 2) * 3" `shouldParse` ((IntLiter 1 :+: IntLiter 2) :*: IntLiter 3)

  it "boolean AND" $
    test pExpr "true && false" `shouldParse` (BoolLiter True :&&: BoolLiter False)

  it "boolean OR" $
    test pExpr "true || false" `shouldParse` (BoolLiter True :||: BoolLiter False)

  it "boolean equality" $
    test pExpr "true == false" `shouldParse` (BoolLiter True :==: BoolLiter False)

  it "int equality" $
    test pExpr "1 == 2" `shouldParse` (IntLiter 1 :==: IntLiter 2)

  it "character equality" $
    test pExpr "'a' == 'b'" `shouldParse` (CharLiter 'a' :==: CharLiter 'b')

  it "string equality" $
    test pExpr "\"hello\" == \"world\"" `shouldParse` (StrLiter "hello" :==: StrLiter "world")

  it "negation of a parenthesised expression" $
    test pExpr "-(1 + 2)" `shouldParse` Neg (IntLiter 1 :+: IntLiter 2)

  it "multiple subexpressions involving idents" $
    test pExpr "-(1 * x) + (y / 2)" `shouldParse` (Neg (IntLiter 1 :*: (IdentExpr (Ident "x"))) :+: ((IdentExpr (Ident "y")) :/: IntLiter 2))

  it "expression with len unop" $
    test pExpr "len 5" `shouldParse` Len (IntLiter 5)

  it "expression with ord unop" $
    test pExpr "ord 5" `shouldParse` Ord (IntLiter 5)

  it "expression with chr unop" $
    test pExpr "chr 5" `shouldParse` Chr (IntLiter 5)

  it "nested unaries" $
    test pExpr "chr (ord 5)" `shouldParse` Chr (Ord (IntLiter 5))

  it "len unop applied to no arguments" $
    test pExpr `shouldFailOn` "len"

  it "chr unop applied to a paranthesised expression" $
    test pExpr "chr(5 * (6 + 7))" `shouldParse` Chr (IntLiter 5 :*: (IntLiter 6 :+: IntLiter 7))

  it "array elem with dimension of 1" $
    test pArrayElem "arr[5]" `shouldParse` ArrayElem (Ident "arr") [IntLiter 5]

  it "identifier _" $
    test pIdent "_" `shouldParse` Ident "_"

  it "single char identifier" $
    test pIdent "a" `shouldParse` Ident "a"

  it "identifier multichar identifier" $
    test pIdent "abc" `shouldParse` Ident "abc"

  it "empty char literal" $
    test pChar "' '" `shouldParse` CharLiter ' '

  it "char literal" $
    test pChar "'a'" `shouldParse` CharLiter 'a'