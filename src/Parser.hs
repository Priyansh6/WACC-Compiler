{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified AST as AST
import qualified Data.Text as T 

type Parser = Parsec Void T.Text

pBool :: Parser AST.Expr
pBool = choice 
  [ AST.BoolLiter True <$ string "true",
    AST.BoolLiter False <$ string "false" ]