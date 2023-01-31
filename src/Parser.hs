{-# LANGUAGE OverloadedStrings #-}

module Parser (Parser, pBool) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified AST 
import qualified Data.Text as T 

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

pBool :: Parser AST.Expr
pBool = choice 
  [ AST.BoolLiter True <$ lexeme (string "true"),
    AST.BoolLiter False <$ lexeme (string "false") ]