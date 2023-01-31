{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified AST 
import qualified Parser
import Test.Hspec
import Text.Megaparsec
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