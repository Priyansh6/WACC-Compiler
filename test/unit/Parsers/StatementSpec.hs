{-# LANGUAGE OverloadedStrings #-}

module Parsers.StatementSpec (spec) where

import AST
import Parsers.Test
import Statements
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  it "skip" $ do
    test pSkip "skip" `shouldParse` Skip

  it "free expr" $ do
    test pFree "free 1" `shouldParse` Free (IntLiter 1)

  it "fails returning nothing" $ do
    test pReturn `shouldFailOn` "return"

  it "fails without space after exit " $ do
    test pExit `shouldFailOn` "exit("

  it "fails on nested keyword stats" $ do
    test pPrint `shouldFailOn` "print exit 'h'"

  it "calling functions with one argument" $ do
    test pCall "call f(x)" `shouldParse` Call (Ident "f") [IdentExpr (Ident "x")]

  it "calling functions with multiple arguments" $ do
    test pCall "call f(x, y, 'c', 7)" `shouldParse` Call (Ident "f") [IdentExpr (Ident "x"), IdentExpr (Ident "y"), CharLiter 'c', IntLiter 7]

  describe "RVal" $ do
    it "RExpression of integer" $ do
      test pRVal "911" `shouldParse` RExpr (IntLiter 911)

    it "RExpression of string" $ do
      test pRVal "\"hello\"" `shouldParse` RExpr (StrLiter "hello")

    it "RExpression of char" $ do
      test pRVal "'c'" `shouldParse` RExpr (CharLiter 'c')

    it "ArrayLiter" $ do
      test pRVal "[true,false]" `shouldParse` ArrayLiter [BoolLiter True, BoolLiter False]

    it "Empty ArrayLiter" $ 
      test pRVal "[]" `shouldParse` ArrayLiter []

  describe "Declarative Assignments" $ do
    it "dec assign a LExpr bool" $ do
      test pDecAssign "bool a = true" `shouldParse` DecAssign WBool (Ident "a") (RExpr (BoolLiter True))

    it "dec assign a RExpr integer" $ do
      test pDecAssign "int bob = 1" `shouldParse` DecAssign WInt (Ident "bob") (RExpr (IntLiter 1))

    it "dec assign a RExpr string" $ do
      test pDecAssign "string _0 = \"hello\"" `shouldParse` DecAssign WStr (Ident "_0") (RExpr (StrLiter "hello"))

    xit "dec assign a RExpr char" $ do
      test pDecAssign "char _q = '\"'" `shouldParse` DecAssign WChar (Ident "_q") (RExpr (CharLiter '\"'))

    xit "dec assign an ArrayLiter" $ do
      test pDecAssign "char[] Zy_ = ['\"']" `shouldParse` DecAssign (WArr WChar 1) (Ident "Zy_") (ArrayLiter [CharLiter '\"'])

    it "dec assign a NewPair" $ do
      test pDecAssign "pair(char, bool) _ = newpair('\\0', true)" `shouldParse` DecAssign (WPair WChar WBool) (Ident "_") (NewPair (CharLiter '\0') (BoolLiter True))

    it "dec assign a read pair" $ do
      test pDecAssign "string s = snd p" `shouldParse` DecAssign WStr (Ident "s") (RPair (Snd (LIdent (Ident "p"))))

    xit "dec assign a read pair from array elem" $ do
      test pDecAssign "char sss = fst a[0][1]" `shouldParse` DecAssign WChar (Ident "sss") (RPair (Fst (LArray (ArrayElem (Ident "a") [IntLiter 0, IntLiter 1]))))

  describe "Statement" $ do
    it "skip" $ do
      test pStat "skip" `shouldParse` Skip

    it "free expr" $ do
      test pStat "free 1" `shouldParse` Free (IntLiter 1)

    it "dec assign a LExpr bool" $ do
      test pStat "bool a = true" `shouldParse` DecAssign WBool (Ident "a") (RExpr (BoolLiter True))

    it "dec assign a RExpr integer" $ do
      test pStat "int bob = 1" `shouldParse` DecAssign WInt (Ident "bob") (RExpr (IntLiter 1))

    xit "dec assign a RExpr string" $ do
      test pStat "string _0 = \"hell\'o\"" `shouldParse` DecAssign WStr (Ident "_0") (RExpr (StrLiter "hell'o"))

    xit "dec assign a RExpr char" $ do
      test pStat "char _q = '\"'" `shouldParse` DecAssign WChar (Ident "_q") (RExpr (CharLiter '\"'))

    xit "dec assign an ArrayLiter" $ do
      test pStat "char[] Zy_ = ['\"']" `shouldParse` DecAssign (WArr WChar 1) (Ident "Zy_") (ArrayLiter [CharLiter '\"'])

    it "dec assign a NewPair" $ do
      test pStat "pair(char, bool) _ = newpair('\\0', true)" `shouldParse` DecAssign (WPair WChar WBool) (Ident "_") (NewPair (CharLiter '\0') (BoolLiter True))

    it "dec assign a read pair" $ do
      test pStat "string s = snd p" `shouldParse` DecAssign WStr (Ident "s") (RPair (Snd (LIdent (Ident "p"))))

  describe "Statements" $ do
    it "skip" $ do
      test pStats "skip" `shouldParse` [Skip]

    it "free expr" $ do
      test pStats "free 1" `shouldParse` [Free (IntLiter 1)]

    it "dec assign a LExpr bool" $ do
      test pStats "bool a = true" `shouldParse` [DecAssign WBool (Ident "a") (RExpr (BoolLiter True))]

    it "dec assign a RExpr integer" $ do
      test pStats "int bob = 1" `shouldParse` [DecAssign WInt (Ident "bob") (RExpr (IntLiter 1))]

    xit "dec assign a RExpr string" $ do
      test pStats "string _0 = \"hell\'o\"" `shouldParse` [DecAssign WStr (Ident "_0") (RExpr (StrLiter "hell'o"))]

    xit "dec assign a RExpr char" $ do
      test pStats "char _q = '\"'" `shouldParse` [DecAssign WChar (Ident "_q") (RExpr (CharLiter '\"'))]

    xit "dec assign an ArrayLiter" $ do
      test pStats "char[] Zy_ = ['\"']" `shouldParse` [DecAssign (WArr WChar 1) (Ident "Zy_") (ArrayLiter [CharLiter '\"'])]

    it "dec assign a NewPair" $ do
      test pStats "pair(char, bool) _ = newpair('\\0', true)" `shouldParse` [DecAssign (WPair WChar WBool) (Ident "_") (NewPair (CharLiter '\0') (BoolLiter True))]

    it "dec assign a read pair" $ do
      test pStats "string s = snd p" `shouldParse` [DecAssign WStr (Ident "s") (RPair (Snd (LIdent (Ident "p"))))]

    it "dec assign, then reassign a RExpr integer" $ do
      test pStats "int bob = 1; bob = 2" `shouldParse` [DecAssign WInt (Ident "bob") (RExpr (IntLiter 1)), Assign (LIdent (Ident "bob")) (RExpr (IntLiter 2))]