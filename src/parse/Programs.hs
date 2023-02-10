{-# LANGUAGE OverloadedStrings #-}

module Programs 
  ( program,
    func
  ) 
where

import Control.Monad.Combinators
import Expressions (ident) 
import Parser (Parser)
import ProgramConstructors
import Statements (stats, wtype)

import qualified AST 
import qualified Lexer as L

program :: Parser AST.Program
program = mkProgram ("begin" *> many func) (stats <* "end")

func :: Parser AST.Func
func = mkFunc wtype ident (L.parens pParamList) ("is" *> stats <* "end")

pParamList :: Parser [(AST.WType, AST.Ident)]
pParamList = pParam `sepBy` ","
  where
    pParam :: Parser (AST.WType, AST.Ident)
    pParam = (,) <$> wtype <*> ident