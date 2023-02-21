{-# LANGUAGE OverloadedStrings #-}

module UnitTest.Syntax.ArrayElemSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec

import AST
import Syntax.Expressions (arrayElem, expr)
import UnitTest.Syntax.Test

spec :: Spec
spec = do
  describe "ArrayElem" $ do
    it "indexed once by an int" $
      test arrayElem "arr[1]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5)] (1, 1)

    it "indexed more than once by an int literal" $
      test arrayElem "arr[1][2]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5), IntLiter 2 (1, 8)] (1, 1)

    it "indexed once by an expression" $
      test arrayElem "arr[1 + 2]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7)] (1, 1)

    it "indexed more than once by an expression" $
      test arrayElem "arr[1 + 2][2 + 3]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7), (IntLiter 2 (1, 12) :+: IntLiter 3 (1, 16)) (1, 14)] (1, 1)

    it "indexed by an ident" $
      test arrayElem "arr[x]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IdentExpr (Ident "x" (1, 5)) (1, 5)] (1, 1)

    it "indexed by another array elem" $
      test arrayElem "arr1[arr2[0]]" `shouldParse` ArrayElem (Ident "arr1" (1, 1)) [ArrayExpr (ArrayElem (Ident "arr2" (1, 6)) [IntLiter 0 (1, 11)] (1, 6)) (1, 6)] (1, 1)

    it "fails with empty index" $
      test arrayElem `shouldFailOn` "arr[]"

    it "parses an array elem indexed once by an int literal as an expression" $
      test expr "arr[1]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5)] (1, 1)) (1, 1)

    it "parses an array elem indexed more than once by an int literal as an expression" $
      test expr "arr[1][2]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5), IntLiter 2 (1, 8)] (1, 1)) (1, 1)

    it "parses an array elem indexed once by an expression as an expression" $
      test expr "arr[1 + 2]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7)] (1, 1)) (1, 1)

    it "parses an array elem indexed more than once by an expression as an expression" $
      test expr "arr[1 + 2][2 + 3]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7), (IntLiter 2 (1, 12) :+: IntLiter 3 (1, 16)) (1, 14)] (1, 1)) (1, 1)

    it "parses an array elem indexed by an ident as an expression" $
      test expr "arr[x]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [IdentExpr (Ident "x" (1, 5)) (1, 5)] (1, 1)) (1, 1)

    it "parses array elems indexed by another array elem as an expression" $
      test expr "arr1[arr2[0]]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr1" (1, 1)) [ArrayExpr (ArrayElem (Ident "arr2" (1, 6)) [IntLiter 0 (1, 11)] (1, 6)) (1, 6)] (1, 1)) (1, 1)

    xit "can't parse an array elem with an empty index as an expression" $
      test expr `shouldFailOn` "arr[]"