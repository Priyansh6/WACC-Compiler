{-# LANGUAGE OverloadedStrings #-}

module Syntax.Program 
  ( program,
    func
  ) 
where

import Control.Monad.Combinators

import qualified AST 
import qualified Lexer as L
import Syntax.Expressions (ident) 
import Syntax.Parser (Parser)
import Syntax.ProgramConstructors
import Syntax.Statements (stats, wtype)

program :: Parser AST.Program
program = mkProgram ("begin" *> many func) (stats <* "end")

func :: Parser AST.Func
func = mkFunc wtype ident (L.parens pParamList) ("is" *> stats <* "end")

pParamList :: Parser [(AST.WType, AST.Ident)]
pParamList = pParam `sepBy` ","
  where
    pParam :: Parser (AST.WType, AST.Ident)
    pParam = (,) <$> wtype <*> ident