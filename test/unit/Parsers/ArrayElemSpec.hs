{-# LANGUAGE OverloadedStrings #-}

module Parsers.ArrayElemSpec (spec) where

import AST
import Expressions (mkArrayElem, pExpr)
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  describe "ArrayElem" $ do
    it "indexed once by an int" $
      test mkArrayElem "arr[1]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5)] (1, 1)

    it "indexed more than once by an int literal" $
      test mkArrayElem "arr[1][2]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5), IntLiter 2 (1, 8)] (1, 1)

    it "indexed once by an expression" $
      test mkArrayElem "arr[1 + 2]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7)] (1, 1)

    it "indexed more than once by an expression" $
      test mkArrayElem "arr[1 + 2][2 + 3]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7), (IntLiter 2 (1, 12) :+: IntLiter 3 (1, 16)) (1, 14)] (1, 1)

    it "indexed by an ident" $
      test mkArrayElem "arr[x]" `shouldParse` ArrayElem (Ident "arr" (1, 1)) [IdentExpr (Ident "x" (1, 5)) (1, 5)] (1, 1)

    it "indexed by another array elem" $
      test mkArrayElem "arr1[arr2[0]]" `shouldParse` ArrayElem (Ident "arr1" (1, 1)) [ArrayExpr (ArrayElem (Ident "arr2" (1, 6)) [IntLiter 0 (1, 11)] (1, 6)) (1, 6)] (1, 1)

    it "fails with empty index" $
      test mkArrayElem `shouldFailOn` "arr[]"

    it "parses an array elem indexed once by an int literal as an expression" $
      test pExpr "arr[1]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5)] (1, 1)) (1, 1)

    it "parses an array elem indexed more than once by an int literal as an expression" $
      test pExpr "arr[1][2]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [IntLiter 1 (1, 5), IntLiter 2 (1, 8)] (1, 1)) (1, 1)

    it "parses an array elem indexed once by an expression as an expression" $
      test pExpr "arr[1 + 2]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7)] (1, 1)) (1, 1)

    it "parses an array elem indexed more than once by an expression as an expression" $
      test pExpr "arr[1 + 2][2 + 3]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [(IntLiter 1 (1, 5) :+: IntLiter 2 (1, 9)) (1, 7), (IntLiter 2 (1, 12) :+: IntLiter 3 (1, 16)) (1, 14)] (1, 1)) (1, 1)

    it "parses an array elem indexed by an ident as an expression" $
      test pExpr "arr[x]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr" (1, 1)) [IdentExpr (Ident "x" (1, 5)) (1, 5)] (1, 1)) (1, 1)

    it "parses array elems indexed by another array elem as an expression" $
      test pExpr "arr1[arr2[0]]" `shouldParse` ArrayExpr (ArrayElem (Ident "arr1" (1, 1)) [ArrayExpr (ArrayElem (Ident "arr2" (1, 6)) [IntLiter 0 (1, 11)] (1, 6)) (1, 6)] (1, 1)) (1, 1)

    xit "can't parse an array elem with an empty index as an expression" $
      test pExpr `shouldFailOn` "arr[]"