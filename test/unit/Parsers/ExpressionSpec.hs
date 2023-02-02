{-# LANGUAGE OverloadedStrings #-}

module Parsers.ExpressionSpec (spec) where

import AST
import qualified Expressions
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do
  it "int multiplication" $
    parse Expressions.pExpr "" "1 * 2" `shouldParse` (IntLiter 1 :*: IntLiter 2)

  it "int division" $
    parse Expressions.pExpr "" "1 / 2" `shouldParse` (IntLiter 1 :/: IntLiter 2)

  it "int modulo" $
    parse Expressions.pExpr "" "1 % 2" `shouldParse` (IntLiter 1 :%: IntLiter 2)

  it "int addition" $
    parse Expressions.pExpr "" "1 + 2" `shouldParse` (IntLiter 1 :+: IntLiter 2)

  it "int subtraction" $
    parse Expressions.pExpr "" "1 - 2" `shouldParse` (IntLiter 1 :-: IntLiter 2)

  it "binary op without whitespace" $
    parse Expressions.pExpr "" "1-2" `shouldParse` (IntLiter 1 :-: IntLiter 2)

  it "binary ops with higher precedence than another" $
    parse Expressions.pExpr "" "1 + 2 * 3" `shouldParse` (IntLiter 1 :+: (IntLiter 2 :*: IntLiter 3))

  it "binary ops of same precedence with parentheses" $
    parse Expressions.pExpr "" "(1 * 2) * 3" `shouldParse` ((IntLiter 1 :*: IntLiter 2) :*: IntLiter 3)

  it "binary ops of different precedence with parentheses" $
    parse Expressions.pExpr "" "(1 + 2) * 3" `shouldParse` ((IntLiter 1 :+: IntLiter 2) :*: IntLiter 3)

  it "boolean AND" $
    parse Expressions.pExpr "" "true && false" `shouldParse` (BoolLiter True :&&: BoolLiter False)

  it "boolean OR" $
    parse Expressions.pExpr "" "true || false" `shouldParse` (BoolLiter True :||: BoolLiter False)

  it "boolean equality" $
    parse Expressions.pExpr "" "true == false" `shouldParse` (BoolLiter True :==: BoolLiter False)

  it "int equality" $
    parse Expressions.pExpr "" "1 == 2" `shouldParse` (IntLiter 1 :==: IntLiter 2)

  it "character equality" $
    parse Expressions.pExpr "" "'a' == 'b'" `shouldParse` (CharLiter 'a' :==: CharLiter 'b')

  it "string equality" $
    parse Expressions.pExpr "" "\"hello\" == \"world\"" `shouldParse` (StrLiter "hello" :==: StrLiter "world")

  it "negation of a parenthesised expression" $
    parse Expressions.pExpr "" "-(1 + 2)" `shouldParse` Neg (IntLiter 1 :+: IntLiter 2)

  it "multiple subexpressions involving idents" $
    parse Expressions.pExpr "" "-(1 * x) + (y / 2)" `shouldParse` (Neg (IntLiter 1 :*: (IdentExpr (Ident "x"))) :+: ((IdentExpr (Ident "y")) :/: IntLiter 2))
