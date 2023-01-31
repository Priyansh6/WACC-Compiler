{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Parser,
    pInt,
    pBool,
    pChar,
    pString,
  )
where

import qualified AST 
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

pBool :: Parser AST.Expr
pBool =
  choice
    [ AST.BoolLiter True <$ lexeme (string "true"),
      AST.BoolLiter False <$ lexeme (string "false")
    ]

pInt :: Parser AST.Expr
pInt = AST.IntLiter <$> L.signed (return ()) integer
  where
    integer = lexeme L.decimal

pChar :: Parser AST.Expr
pChar = AST.CharLiter <$> between (char '\'') (lexeme (char '\'')) L.charLiteral

pString :: Parser AST.Expr
pString = do
  s <- lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))
  return (AST.StrLiter (T.pack s))