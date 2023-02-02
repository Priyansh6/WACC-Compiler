{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified AST 
import qualified Parser
import Test.Hspec
import Text.Megaparsec
import qualified Parser
import qualified Expressions 

spec :: Spec
spec = do
  -- bool-liter
  it "parses true to a BoolLiter True" $ do
    parseMaybe Expressions.pBool "true" `shouldBe` Just (AST.BoolLiter True)

  it "parses false to a BoolLiter False" $
    parseMaybe Expressions.pBool "false" `shouldBe` Just (AST.BoolLiter False)

  it "can't parse strings prefixed with a bool to BoolLiter" $
    parseMaybe Expressions.pBool "truetrue" `shouldBe` Nothing

  it "can't parse non-bools to BoolLiter" $
    parseMaybe Expressions.pBool "e" `shouldBe` Nothing

  it "parses bools with trailing whitespace" $
    parseMaybe Expressions.pBool "true " `shouldBe` Just (AST.BoolLiter True)

  -- int-liter
  it "parses unsigned integer literal" $
    parseMaybe Expressions.pInt "123" `shouldBe` Just (AST.IntLiter 123)

  it "parses positive integer literal" $
    parseMaybe Expressions.pInt "+123" `shouldBe` Just (AST.IntLiter 123)

  it "parses negative integer literal" $
    parseMaybe Expressions.pInt "-123" `shouldBe` Just (AST.IntLiter (-123))

  it "can't parse only a sign" $
    parseMaybe Expressions.pInt "+" `shouldBe` Nothing

  it "can't parse nothing" $
    parseMaybe Expressions.pInt "" `shouldBe` Nothing

  -- char-liter
  it "parses a character" $
    parseMaybe Expressions.pChar "'a'" `shouldBe` Just (AST.CharLiter 'a')

  it "can't parse empty character" $
    parseMaybe Expressions.pChar "''" `shouldBe` Nothing

  it "can't parse multiple characters" $
    parseMaybe Expressions.pChar "'aa'" `shouldBe` Nothing

  it "can't parse nothing" $
    parseMaybe Expressions.pChar "" `shouldBe` Nothing

  -- str-liter
  it "parses a string" $
    parseMaybe Expressions.pString "\"aa\"" `shouldBe` Just (AST.StrLiter "aa")

  it "parses empty string" $
    parseMaybe Expressions.pString "\"\"" `shouldBe` Just (AST.StrLiter "")

  it "can't parse incorrect string notation" $
    parseMaybe Expressions.pString "\"a\"\"" `shouldBe` Nothing

  it "can't parse nothing" $
    parseMaybe Expressions.pString "" `shouldBe` Nothing

  -- pair-liter
  it "parses a pair literal" $
    parseMaybe Expressions.pPairLit "null" `shouldBe` Just AST.PairLiter

  it "can't the empty string" $
    parseMaybe Expressions.pPairLit "" `shouldBe` Nothing

  -- ident
  it "parses an identifier" $
    parseMaybe Parser.pIdent "hello" `shouldBe` Just (AST.Ident "hello")

  it "parses an single letter identifier" $
    parseMaybe Parser.pIdent "a" `shouldBe` Just (AST.Ident "a")
     
  it "parses an underscore identifier" $
    parseMaybe Parser.pIdent "_" `shouldBe` Just (AST.Ident "_")
 
  it "parses an underscore prefixed identifier" $
    parseMaybe Parser.pIdent "_a" `shouldBe` Just (AST.Ident "_a")

  it "parses an identifier" $
    parseMaybe Parser.pIdent "aB1_" `shouldBe` Just (AST.Ident "aB1_")

  it "can't parse an invalid identifier" $
    parseMaybe Parser.pIdent "1" `shouldBe` Nothing

  it "can't parse an identifier followed by an invalid char" $
    parseMaybe Parser.pIdent "hello[]" `shouldBe` Nothing

  -- array elem
  it "parses an array elem indexed once by an int literal" $
    parseMaybe Expressions.pArrayElem "arr[1]" `shouldBe` Just (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1])

  it "parses an array elem indexed more than once by an int literal" $
    parseMaybe Expressions.pArrayElem "arr[1][2]" `shouldBe` Just (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1, AST.IntLiter 2])

  it "parses an array elem indexed once by an expression" $
    parseMaybe Expressions.pArrayElem "arr[1 + 2]" `shouldBe` Just (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1 AST.:+: AST.IntLiter 2])

  it "parses an array elem indexed more than once by an expression" $
    parseMaybe Expressions.pArrayElem "arr[1 + 2][2 + 3]" `shouldBe` Just (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1 AST.:+: AST.IntLiter 2, AST.IntLiter 2 AST.:+: AST.IntLiter 3])

  it "parses an array elem indexed by an ident" $
    parseMaybe Expressions.pArrayElem "arr[x]" `shouldBe` Just (AST.ArrayElem (AST.Ident "arr") [AST.IdentExpr (AST.Ident "x")])

  it "parses array elems indexed by another array elem" $
    parseMaybe Expressions.pArrayElem "arr1[arr2[0]]" `shouldBe` Just (AST.ArrayElem (AST.Ident "arr1") [AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr2") [AST.IntLiter 0])])

  it "can't parse an array elem with an empty index" $
    parseMaybe Expressions.pArrayElem "arr[]" `shouldBe` Nothing

  -- expr
  it "parses multiplication of two ints" $
    parseMaybe Expressions.pExpr "1 * 2" `shouldBe` Just (AST.IntLiter 1 AST.:*: AST.IntLiter 2)

  it "parses division of two ints" $
    parseMaybe Expressions.pExpr "1 / 2" `shouldBe` Just (AST.IntLiter 1 AST.:/: AST.IntLiter 2)

  it "parses modulo of two ints" $
    parseMaybe Expressions.pExpr "1 % 2" `shouldBe` Just (AST.IntLiter 1 AST.:%: AST.IntLiter 2)

  it "parses addition of two ints" $
    parseMaybe Expressions.pExpr "1 + 2" `shouldBe` Just (AST.IntLiter 1 AST.:+: AST.IntLiter 2)

  it "parses subtraction of two ints" $
    parseMaybe Expressions.pExpr "1 - 2" `shouldBe` Just (AST.IntLiter 1 AST.:-: AST.IntLiter 2)

  it "parses binary operation on two ints with no whitespace" $
    parseMaybe Expressions.pExpr "1-2" `shouldBe` Just (AST.IntLiter 1 AST.:-: AST.IntLiter 2)

  it "parses binary operations where one has higher precedence than another" $
    parseMaybe Expressions.pExpr "1 + 2 * 3" `shouldBe` Just (AST.IntLiter 1 AST.:+: (AST.IntLiter 2 AST.:*: AST.IntLiter 3))

  it "parses binary operations of the same precedence with parentheses" $
    parseMaybe Expressions.pExpr "(1 * 2) * 3" `shouldBe` Just ((AST.IntLiter 1 AST.:*: AST.IntLiter 2) AST.:*: AST.IntLiter 3)

  it "parses binary operations of different precedence with parentheses" $
    parseMaybe Expressions.pExpr "(1 + 2) * 3" `shouldBe` Just ((AST.IntLiter 1 AST.:+: AST.IntLiter 2) AST.:*: AST.IntLiter 3)

  it "parses and of two bools" $
    parseMaybe Expressions.pExpr "true && false" `shouldBe` Just (AST.BoolLiter True AST.:&&: AST.BoolLiter False)

  it "parses or of two bools" $
    parseMaybe Expressions.pExpr "true || false" `shouldBe` Just (AST.BoolLiter True AST.:||: AST.BoolLiter False)

  it "parses equals of two bools" $
    parseMaybe Expressions.pExpr "true == false" `shouldBe` Just (AST.BoolLiter True AST.:==: AST.BoolLiter False)

  it "parses equals of two ints" $
    parseMaybe Expressions.pExpr "1 == 2" `shouldBe` Just (AST.IntLiter 1 AST.:==: AST.IntLiter 2)

  it "parses equals of two chars" $
    parseMaybe Expressions.pExpr "'a' == 'b'" `shouldBe` Just (AST.CharLiter 'a' AST.:==: AST.CharLiter 'b')

  it "parses equals of two strings" $
    parseMaybe Expressions.pExpr "\"hello\" == \"world\"" `shouldBe` Just (AST.StrLiter "hello" AST.:==: AST.StrLiter "world")

  it "parses negation of a parenthesised expression" $
    parseMaybe Expressions.pExpr "-(1 + 2)" `shouldBe` Just (AST.Neg (AST.IntLiter 1 AST.:+: AST.IntLiter 2))

  it "parses expressions with multiple subexpressions involving idents" $
    parseMaybe Expressions.pExpr "-(1 * x) + (y / 2)" `shouldBe` Just (AST.Neg (AST.IntLiter 1 AST.:*: (AST.IdentExpr (AST.Ident "x"))) AST.:+: ((AST.IdentExpr (AST.Ident "y")) AST.:/: AST.IntLiter 2))

  it "parses an array elem indexed once by an int literal as an expression" $
    parseMaybe Expressions.pExpr "arr[1]" `shouldBe` Just (AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1]))

  it "parses an array elem indexed more than once by an int literal as an expression" $
    parseMaybe Expressions.pExpr "arr[1][2]" `shouldBe` Just (AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1, AST.IntLiter 2]))

  it "parses an array elem indexed once by an expression as an expression" $
    parseMaybe Expressions.pExpr "arr[1 + 2]" `shouldBe` Just (AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1 AST.:+: AST.IntLiter 2]))

  it "parses an array elem indexed more than once by an expression as an expression" $
    parseMaybe Expressions.pExpr "arr[1 + 2][2 + 3]" `shouldBe` Just (AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1 AST.:+: AST.IntLiter 2, AST.IntLiter 2 AST.:+: AST.IntLiter 3]))

  it "parses an array elem indexed by an ident as an expression" $
    parseMaybe Expressions.pExpr "arr[x]" `shouldBe` Just (AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr") [AST.IdentExpr (AST.Ident "x")]))

  it "parses array elems indexed by another array elem as an expression" $
    parseMaybe Expressions.pExpr "arr1[arr2[0]]" `shouldBe` Just (AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr1") [AST.ArrayExpr (AST.ArrayElem (AST.Ident "arr2") [AST.IntLiter 0])]))

  it "can't parse an array elem with an empty index as an expression" $
    parseMaybe Expressions.pExpr "arr[]" `shouldBe` Nothing
