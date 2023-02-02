{-# LANGUAGE OverloadedStrings #-}

module Parsers.ArrayElemSpec (spec) where

import qualified AST
import qualified Expressions
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

-- parseExp = parse Expressions.pArrayElem ""

spec :: Spec
spec = do
  describe "ArrayElem" $ do
    it "indexed once by an int" $
      parse Expressions.pArrayElem "" "arr[1]" `shouldParse` (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1])

    it "indexed more than once by an int literal" $
      parse Expressions.pArrayElem "" "arr[1][2]" `shouldParse` (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1, AST.IntLiter 2])

    it "indexed once by an expression" $
      parse Expressions.pArrayElem "" "arr[1 + 2]" `shouldParse` (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1 AST.:+: AST.IntLiter 2])

    it "indexed more than once by an expression" $
      parse Expressions.pArrayElem "" "arr[1 + 2][2 + 3]" `shouldParse` (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1 AST.:+: AST.IntLiter 2, AST.IntLiter 2 AST.:+: AST.IntLiter 3])

    it "indexed by an ident" $
      parse Expressions.pArrayElem "" "arr[x]" `shouldParse` (AST.ArrayElem (AST.Ident "arr") [AST.IdentExpr (AST.Ident "x")])

    it "indexed by another array elem" $
      parse Expressions.pArrayElem "" "arr1[arr2[0]]" `shouldParse` (AST.ArrayElem (AST.Ident "arr1") [AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr2") [AST.IntLiter 0])])

    it "fails with empty index" $
      parse Expressions.pArrayElem "" `shouldFailOn` "arr[]"
