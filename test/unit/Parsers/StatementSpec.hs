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
    test mkFree "free 1" `shouldParse` Free (IntLiter 1 (1, 6)) (1, 1)

  it "fails returning nothing" $ do
    test mkReturn `shouldFailOn` "return" 

  it "fails without space after exit " $ do
    test mkExit `shouldFailOn` "exit("

  it "fails on nested keyword stats" $ do
    test pPrint `shouldFailOn` "print exit 'h'"

  it "calling functions with one argument" $ do
    test mkCall "call f(x)" `shouldParse` Call (Ident "f" (1, 6)) [IdentExpr (Ident "x" (1, 8)) (1, 8)] (1, 1)

  it "calling functions with multiple arguments" $ do
    test mkCall "call f(x, y, 'c', 7)" `shouldParse` Call (Ident "f" (1, 6)) [IdentExpr (Ident "x" (1, 8)) (1, 8), IdentExpr (Ident "y" (1, 11)) (1, 11), CharLiter 'c' (1, 14), IntLiter 7 (1, 19)] (1, 1)

  describe "RVal" $ do
    it "RExpression of integer" $ do
      test pRVal "911" `shouldParse` RExpr (IntLiter 911 (1, 1))

    it "RExpression of string" $ do
      test pRVal "\"hello\"" `shouldParse` RExpr (StrLiter "hello" (1, 1))

    it "RExpression of char" $ do
      test pRVal "'c'" `shouldParse` RExpr (CharLiter 'c' (1, 1))

    it "ArrayLiter" $ do
      test pRVal "[true,false]" `shouldParse` ArrayLiter [BoolLiter True (1, 2), BoolLiter False (1, 7)] (1, 1)

    it "Empty ArrayLiter" $ 
      test pRVal "[]" `shouldParse` ArrayLiter [] (1, 1)

  describe "Declarative Assignments" $ do
    it "dec assign a LExpr bool" $ do
      test mkDecAssign "bool a = true" `shouldParse` DecAssign WBool (Ident "a" (1, 6)) (RExpr (BoolLiter True (1, 10))) (1, 1)

    it "dec assign a RExpr integer" $ do
      test mkDecAssign "int bob = 1" `shouldParse` DecAssign WInt (Ident "bob" (1, 5)) (RExpr (IntLiter 1 (1, 11))) (1, 1)

    it "dec assign a RExpr string" $ do
      test mkDecAssign "string _0 = \"hello\"" `shouldParse` DecAssign WStr (Ident "_0" (1, 8)) (RExpr (StrLiter "hello" (1, 13))) (1, 1)

    xit "dec assign a RExpr char" $ do
      test mkDecAssign "char _q = '\"'" `shouldParse` DecAssign WChar (Ident "_q" (1, 6)) (RExpr (CharLiter '\"' (1, 11))) (1, 1)

    xit "dec assign an ArrayLiter" $ do
      test mkDecAssign "char[] Zy_ = ['\"']" `shouldParse` DecAssign (WArr WChar 1) (Ident "Zy_" (1, 8)) (ArrayLiter [CharLiter '\"' (1, 15)] (1, 14)) (1, 1)

    it "dec assign a NewPair" $ do
      test mkDecAssign "pair(char, bool) _ = newpair('\\0', true)" `shouldParse` DecAssign (WPair WChar WBool) (Ident "_" (1, 18)) (NewPair (CharLiter '\0' (1, 30)) (BoolLiter True (1, 36)) (1, 22)) (1, 1)

    it "dec assign a read pair" $ do
      test mkDecAssign "string s = snd p" `shouldParse` DecAssign WStr (Ident "s" (1, 8)) (RPair (Snd (LIdent (Ident "p" (1, 16))) (1, 12))) (1, 1)

    xit "dec assign a read pair from array elem" $ do
      test mkDecAssign "char sss = fst a[0][1]" `shouldParse` DecAssign WChar (Ident "sss" (1, 6)) (RPair (Fst (LArray (ArrayElem (Ident "a" (1, 16)) [IntLiter 0 (1, 18), IntLiter 1 (1, 21)] (1, 17))) (1, 12))) (1, 1)

  describe "Statement" $ do
    it "skip" $ do
      test pStat "skip" `shouldParse` Skip

    it "free expr" $ do
      test pStat "free 1" `shouldParse` Free (IntLiter 1 (1, 6)) (1, 1)

    it "dec assign a LExpr bool" $ do
      test pStat "bool a = true" `shouldParse` DecAssign WBool (Ident "a" (1, 6)) (RExpr (BoolLiter True (1, 10))) (1, 1)

    it "dec assign a RExpr integer" $ do
      test pStat "int bob = 1" `shouldParse` DecAssign WInt (Ident "bob" (1, 5)) (RExpr (IntLiter 1 (1, 11))) (1, 1)

    xit "dec assign a RExpr string" $ do
      test pStat "string _0 = \"hell\'o\"" `shouldParse` DecAssign WStr (Ident "_0" (1, 8)) (RExpr (StrLiter "hell'o" (1, 13))) (1, 1)

    it "dec assign a RExpr char" $ do
      test pStat "char _q = ' '" `shouldParse` DecAssign WChar (Ident "_q" (1, 6)) (RExpr (CharLiter ' ' (1, 11))) (1, 1)

    xit "dec assign an ArrayLiter" $ do
      test pStat "char[] Zy_ = ['\"']" `shouldParse` DecAssign (WArr WChar 1) (Ident "Zy_" (1, 8)) (ArrayLiter [CharLiter '\"' (1, 15)] (1, 14)) (1, 1)

    it "dec assign a NewPair" $ do
      test pStat "pair(char, bool) _ = newpair('\\0', true)" `shouldParse` DecAssign (WPair WChar WBool) (Ident "_" (1, 18)) (NewPair (CharLiter '\0' (1, 30)) (BoolLiter True (1, 36)) (1, 22)) (1, 1)

    it "dec assign a read pair" $ do
      test pStat "string s = snd p" `shouldParse` DecAssign WStr (Ident "s" (1, 8)) (RPair (Snd (LIdent (Ident "p" (1, 16))) (1, 12))) (1, 1)

  describe "Statements" $ do
    it "skip" $ do
      test pStats "skip" `shouldParse` [Skip]

    it "free expr" $ do
      test pStats "free 1" `shouldParse` [Free (IntLiter 1 (1, 6)) (1, 1)]

    it "dec assign a LExpr bool" $ do
      test pStats "bool a = true" `shouldParse` [DecAssign WBool (Ident "a" (1, 6)) (RExpr (BoolLiter True (1, 10))) (1, 1)]

    it "dec assign a RExpr integer" $ do
      test pStats "int bob = 1" `shouldParse` [DecAssign WInt (Ident "bob" (1, 5)) (RExpr (IntLiter 1 (1, 11))) (1, 1)]

    xit "dec assign a RExpr string" $ do
      test pStats "string _0 = \"hell\'o\"" `shouldParse` [DecAssign WStr (Ident "_0" (1, 8)) (RExpr (StrLiter "hell'o" (1, 13))) (1, 1)]

    xit "dec assign a RExpr char" $ do
      test pStats "char _q = '\"'" `shouldParse` [DecAssign WChar (Ident "_q" (1, 6)) (RExpr (CharLiter ' ' (1, 11))) (1, 1)]

    xit "dec assign an ArrayLiter" $ do
      test pStats "char[] Zy_ = ['\"']" `shouldParse` [DecAssign (WArr WChar 1) (Ident "Zy_" (1, 8)) (ArrayLiter [CharLiter '\"' (1, 15)] (1, 14)) (1, 1)]

    it "dec assign a NewPair" $ do
      test pStats "pair(char, bool) _ = newpair('\\0', true)" `shouldParse` [DecAssign (WPair WChar WBool) (Ident "_" (1, 18)) (NewPair (CharLiter '\0' (1, 30)) (BoolLiter True (1, 36)) (1, 22)) (1, 1)]

    it "dec assign a read pair" $ do
      test pStats "string s = snd p" `shouldParse` [DecAssign WStr (Ident "s" (1, 8)) (RPair (Snd (LIdent (Ident "p" (1, 16))) (1, 12))) (1, 1)]

    it "dec assign, then reassign a RExpr integer" $ do
      test pStats "int bob = 1; bob = 2" `shouldParse` [DecAssign WInt (Ident "bob" (1, 5)) (RExpr (IntLiter 1 (1, 11))) (1, 1), Assign (LIdent (Ident "bob" (1, 14))) (RExpr (IntLiter 2 (1, 20))) (1, 14)]