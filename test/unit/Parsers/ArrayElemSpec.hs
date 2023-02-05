{-# LANGUAGE OverloadedStrings #-}

module Parsers.ArrayElemSpec (spec) where

import AST
import Expressions
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  describe "ArrayElem" $ do
    it "indexed once by an int" $
      test pArrayElem "arr[1]" `shouldParse` (ArrayElem (Ident "arr") [IntLiter 1])

    it "indexed more than once by an int literal" $
      test pArrayElem "arr[1][2]" `shouldParse` (ArrayElem (Ident "arr") [IntLiter 1, IntLiter 2])

    it "indexed once by an expression" $
      test pArrayElem "arr[1 + 2]" `shouldParse` (ArrayElem (Ident "arr") [IntLiter 1 :+: IntLiter 2])

    it "indexed more than once by an expression" $
      test pArrayElem "arr[1 + 2][2 + 3]" `shouldParse` (ArrayElem (Ident "arr") [IntLiter 1 :+: IntLiter 2, IntLiter 2 :+: IntLiter 3])

    it "indexed by an ident" $
      test pArrayElem "arr[x]" `shouldParse` (ArrayElem (Ident "arr") [IdentExpr (Ident "x")])

    it "indexed by another array elem" $
      test pArrayElem "arr1[arr2[0]]" `shouldParse` (ArrayElem (Ident "arr1") [ArrayExpr (ArrayElem (Ident "arr2") [IntLiter 0])])

    it "fails with empty index" $
      test pArrayElem `shouldFailOn` "arr[]"

    it "parses an array elem indexed once by an int literal as an expression" $
      test pExpr "arr[1]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr") [IntLiter 1])

    it "parses an array elem indexed more than once by an int literal as an expression" $
      test pExpr "arr[1][2]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr") [IntLiter 1, IntLiter 2])

    it "parses an array elem indexed once by an expression as an expression" $
      test pExpr "arr[1 + 2]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr") [IntLiter 1 :+: IntLiter 2])

    it "parses an array elem indexed more than once by an expression as an expression" $
      test pExpr "arr[1 + 2][2 + 3]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr") [IntLiter 1 :+: IntLiter 2, IntLiter 2 :+: IntLiter 3])

    it "parses an array elem indexed by an ident as an expression" $
      test pExpr "arr[x]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr") [IdentExpr (Ident "x")])

    it "parses array elems indexed by another array elem as an expression" $
      test pExpr "arr1[arr2[0]]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr1") [ArrayExpr (ArrayElem (Ident "arr2") [IntLiter 0])])

    xit "can't parse an array elem with an empty index as an expression" $
      test pExpr `shouldFailOn` "arr[]"