{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified AST 
import qualified Parser
import Test.Hspec
import Text.Megaparsec
import qualified Parser
import qualified Parser
import qualified Parser

spec :: Spec
spec = do
  -- bool-liter
  it "parses true to a BoolLiter True" $ do
    parseMaybe Parser.pBool "true" `shouldBe` Just (AST.BoolLiter True)

  it "parses false to a BoolLiter False" $
    parseMaybe Parser.pBool "false" `shouldBe` Just (AST.BoolLiter False)

  it "can't parse strings prefixed with a bool to BoolLiter" $
    parseMaybe Parser.pBool "truetrue" `shouldBe` Nothing

  it "can't parse non-bools to BoolLiter" $
    parseMaybe Parser.pBool "e" `shouldBe` Nothing

  it "parses bools with trailing whitespace" $
    parseMaybe Parser.pBool "true " `shouldBe` Just (AST.BoolLiter True)

  -- int-liter
  it "parses unsigned integer literal" $
    parseMaybe Parser.pInt "123" `shouldBe` Just (AST.IntLiter 123)

  it "parses positive integer literal" $
    parseMaybe Parser.pInt "+123" `shouldBe` Just (AST.IntLiter 123)

  it "parses negative integer literal" $
    parseMaybe Parser.pInt "-123" `shouldBe` Just (AST.IntLiter (-123))

  it "can't parse only a sign" $
    parseMaybe Parser.pInt "+" `shouldBe` Nothing

  it "can't parse nothing" $
    parseMaybe Parser.pInt "" `shouldBe` Nothing

  -- char-liter
  it "parses a character" $
    parseMaybe Parser.pChar "'a'" `shouldBe` Just (AST.CharLiter 'a')

  it "can't parse empty character" $
    parseMaybe Parser.pChar "''" `shouldBe` Nothing

  it "can't parse multiple characters" $
    parseMaybe Parser.pChar "'aa'" `shouldBe` Nothing

  it "can't parse nothing" $
    parseMaybe Parser.pChar "" `shouldBe` Nothing

  -- str-liter
  it "parses a string" $
    parseMaybe Parser.pString "\"aa\"" `shouldBe` Just (AST.StrLiter "aa")

  it "parses empty string" $
    parseMaybe Parser.pString "\"\"" `shouldBe` Just (AST.StrLiter "")

  it "can't parse incorrect string notation" $
    parseMaybe Parser.pString "\"a\"\"" `shouldBe` Nothing

  it "can't parse nothing" $
    parseMaybe Parser.pString "" `shouldBe` Nothing

  -- pair-liter
  it "parses a pair literal" $
    parseMaybe Parser.pPairLit "null" `shouldBe` Just AST.PairLiter

  it "can't the empty string" $
    parseMaybe Parser.pPairLit "" `shouldBe` Nothing

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

  -- array elem
  it "parses an array elem indexed once by an int literal" $
    parseMaybe Parser.pArrayElem "arr[1]" `shouldBe` Just (AST.ArrayElem (AST.Ident "arr") [AST.IntLiter 1])

  -- expr
  it "parses multiplication of two ints" $
    parseMaybe Parser.pExpr "1 * 2" `shouldBe` Just (AST.IntLiter 1 AST.:*: AST.IntLiter 2)

  it "parses division of two ints" $
    parseMaybe Parser.pExpr "1 / 2" `shouldBe` Just (AST.IntLiter 1 AST.:/: AST.IntLiter 2)

  it "parses modulo of two ints" $
    parseMaybe Parser.pExpr "1 % 2" `shouldBe` Just (AST.IntLiter 1 AST.:%: AST.IntLiter 2)

  it "parses addition of two ints" $
    parseMaybe Parser.pExpr "1 + 2" `shouldBe` Just (AST.IntLiter 1 AST.:+: AST.IntLiter 2)

  it "parses subtraction of two ints" $
    parseMaybe Parser.pExpr "1 - 2" `shouldBe` Just (AST.IntLiter 1 AST.:-: AST.IntLiter 2)

  it "parses binary operation on two ints with no whitespace" $
    parseMaybe Parser.pExpr "1-2" `shouldBe` Just (AST.IntLiter 1 AST.:-: AST.IntLiter 2)

  it "parses binary operations where one has higher precedence than another" $
    parseMaybe Parser.pExpr "1 + 2 * 3" `shouldBe` Just (AST.IntLiter 1 AST.:+: (AST.IntLiter 2 AST.:*: AST.IntLiter 3))

  it "parses binary operations of the same precedence with parentheses" $
    parseMaybe Parser.pExpr "(1 * 2) * 3" `shouldBe` Just ((AST.IntLiter 1 AST.:*: AST.IntLiter 2) AST.:*: AST.IntLiter 3)

  it "parses binary operations of different precedence with parentheses" $
    parseMaybe Parser.pExpr "(1 + 2) * 3" `shouldBe` Just ((AST.IntLiter 1 AST.:+: AST.IntLiter 2) AST.:*: AST.IntLiter 3)