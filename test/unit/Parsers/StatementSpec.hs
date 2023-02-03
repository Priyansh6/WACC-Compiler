{-# LANGUAGE OverloadedStrings #-}

module Parsers.StatementSpec (spec) where

import AST
import Statements
import Parsers.Test
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
    test pPrint`shouldFailOn` "print exit 'h'"

  it "assigning variable a value" $ do
    test pDecAssign "int bob = 1" `shouldParse` DecAssign WInt (Ident "bob") (RExpr (IntLiter 1))
