{-# LANGUAGE OverloadedStrings #-}

module Statements 
  (
    pSkip 
  )
where

import qualified AST 
import Control.Monad.Combinators.Expr 
import qualified Data.Text as T
import Parser (Parser, pToken, symbol, pIdent, brackets, parens, lexeme, keyword)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

pSkip :: Parser AST.Stat
pSkip = AST.Skip <$ keyword "skip"
