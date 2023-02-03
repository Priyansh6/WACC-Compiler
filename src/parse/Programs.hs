{-# LANGUAGE OverloadedStrings #-}

module Programs 
  ( pProgram,
    pFunc
  ) 
where

import qualified Data.Text as T
import qualified AST 
import Control.Monad.Combinators.Expr 
import qualified Data.Text as T
import Parser (Parser, pToken, symbol, pIdent, brackets, parens, lexeme, keyword)
import Expressions (pExpr, pArrayElem)
import Statements
import Text.Megaparsec
import Text.Megaparsec.Char

pProgram :: Parser AST.Program
pProgram = AST.Program <$> (keyword "begin" *> many pFunc) <*> (pStats <* keyword "end")

pFunc :: Parser AST.Func
pFunc = try $ AST.Func <$> pWType <*> pIdent <*> parens pParamList <*> (keyword "is" *> pStats <* keyword "end")

pParamList :: Parser [(AST.WType, AST.Ident)]
pParamList = pParam `sepBy` symbol ","
  where
    pParam :: Parser (AST.WType, AST.Ident)
    pParam = (,) <$> pWType <*> pIdent