{-# LANGUAGE OverloadedStrings #-}

module Parsers.TypeSpec (spec) where

import AST
import Statements
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  describe "Base Types" $ do
    it "int keyword type" $
      test pBaseType "int" `shouldParse` WInt

    it "char keyword with space" $
      test pBaseType "char  " `shouldParse` WChar

    it "invalid keyword type" $
      test pBaseType `shouldFailOn` "intchar"

  describe "Pair Types" $ do
    it "pair of int, bool" $
      test pPairType "pair(int,bool)" `shouldParse` WPair WInt WBool

    it "pair type of pair & bool array" $
      test pPairType "pair(pair,bool[])" `shouldParse` WPair WUnit (WArr WBool 1)

    it "pair with whitespace" $
      test pPairType "pair( pair(int,bool)[] , int )" `shouldParse` WPair (WArr (WPair WInt WBool) 1) WInt
    
    it "fails nested pairs" $
      test pPairType `shouldFailOn` "pair(pair(int[], string),pair)"

    it "fails pair with invalid elements" $
      test pPairType `shouldFailOn` "pair(hamish, bool)"
 
    it "fails pair with 1 empty element" $
      test pPairType `shouldFailOn` "pair()"
    
    it "fails pair with 2 empty elements" $
      test pPairType `shouldFailOn` "pair(,)"
    
    it "fails pair with a valid & empty element" $
      test pPairType `shouldFailOn` "pair(int,)"
    
    it "fails untyped pair" $
      test pPairType `shouldFailOn` "pair"
    
    it "fails untyped pair array" $
      test pPairType `shouldFailOn` "pair[]"

  describe "Array Types" $ do
    it "array of strings" $
      test pArrType "string[]" `shouldParse` WArr WStr 1

    it "nested arrays" $
      test pArrType "char[][]" `shouldParse` WArr WChar 2

    it "fails array of untyped pairs" $
      test pArrType `shouldFailOn` "pair[]" 

    it "array of typed pairs" $
      test pArrType "pair(bool, bool)[]" `shouldParse` WArr (WPair WBool WBool) 1

    it "array with whitespace" $
      test pArrType "int  []  " `shouldParse` WArr WInt 1

    it "fails array without brackets" $
      test pArrType `shouldFailOn` "string"

    it "fails on brackets without type" $
      test pArrType `shouldFailOn` "[]"
    
    it "fails with space between brackets" $
      test pArrType `shouldFailOn` "char[ ]"
    
    it "fails on invalid array brackets" $
      test pArrType `shouldFailOn` "char["
    
    it "fails on invalid nested arrays" $
      test pArrType `shouldFailOn` "string[char[]]"
    
    it "fails on array of invalid types" $
      test pArrType `shouldFailOn` "bob[]"

  describe "All Types" $ do
    it "type of int" $
      test pWType "int" `shouldParse` WInt
    
    it "type of array" $
      test pWType "bool[]" `shouldParse` WArr WBool 1
    
    it "type of pairs" $
      test pWType "pair(int, int)" `shouldParse` WPair WInt WInt
    
    it "type with whitespace" $
      test pWType "int [] [] " `shouldParse` WArr WInt 2

    it "fails on nested pairs" $
      test pWType `shouldFailOn` "pair(pair(pair(pair, int), int), int)"
    
    it "fails on invalid types" $
      test pWType `shouldFailOn` "dave[]"
