{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Syntax.Statements 
  ( wtype,
    pBaseType,
    pArrType,
    pPairType,
    stats,
    stat,
    pSkip,
    pDecAssign,
    pAssign,
    pRead,
    lval,
    rval,
    pFree,
    pReturn,
    pCall,
    pPrint,
    pExit,
  ) 
where

import Text.Megaparsec

import qualified AST 
import qualified Lexer as L
import Syntax.Expressions (expr, arrayElem, ident)
import Syntax.Parser (Parser)
import Syntax.StatementConstructors

-- Type Parsers
wtype :: Parser AST.WType
wtype = pArrType <|> pPairType <|> pBaseType 

pBaseType :: Parser AST.WType 
pBaseType = choice 
  [ AST.WInt <$ "int"
  , AST.WBool <$ "bool"
  , AST.WChar <$ "char"
  , AST.WStr <$ "string" ]

pArrType :: Parser AST.WType
pArrType = try $ do
            t <- pBaseType <|> pPairType
            bs <- some "[]"
            let dimension = length bs
            return (AST.WArr t dimension)

pPairType :: Parser AST.WType
pPairType = AST.WPair <$> ("pair" *> "(" *> pPairElemType) <*> ("," *> pPairElemType <* ")")
  where
    pPairElemType :: Parser AST.WType
    pPairElemType = pArrType <|> (AST.WPair AST.WUnit AST.WUnit <$ "pair") <|> pBaseType 

-- Statement Parsers
stats :: Parser AST.Stats
stats = stat `sepBy1` ";"

stat :: Parser AST.Stat
stat = choice 
  [
    pSkip,
    pDecAssign,
    pAssign,
    pRead,
    pFree,
    pReturn,
    pExit,
    pPrint,
    pPrintln,
    pIf, 
    pWhile,
    pBegin
  ]

pSkip :: Parser AST.Stat
pSkip = mkSkip "skip"

pDecAssign :: Parser AST.Stat
pDecAssign = mkDecAssign wtype ident ("=" *> rval)

pAssign :: Parser AST.Stat
pAssign = mkAssign lval ("=" *> rval)

pRead :: Parser AST.Stat
pRead = mkRead $ "read" *> lval

pFree :: Parser AST.Stat
pFree = mkFree $ "free" *> expr

pReturn :: Parser AST.Stat
pReturn = mkReturn $ "return" *> expr

pExit :: Parser AST.Stat
pExit = mkExit $ "exit" *> expr

pPrint :: Parser AST.Stat
pPrint = mkPrint ("print" *> expr)

pPrintln :: Parser AST.Stat
pPrintln = mkPrintln ("println" *> expr)

pIf :: Parser AST.Stat
pIf = mkIf ("if" *> expr) ("then" *> stats) ("else" *> stats <* "fi")

pWhile :: Parser AST.Stat
pWhile = mkWhile ("while" *> expr) ("do" *> stats <* "done")

pBegin :: Parser AST.Stat
pBegin = mkBegin ("begin" *> stats <* "end")

-- LVal Parsers
lval :: Parser AST.LVal 
lval = (AST.LArray <$> arrayElem) <|> (AST.LPair <$> pPairElem) <|> (AST.LIdent <$> ident)

-- pPairElem is common to both LVals and RVals

pPairElem :: Parser AST.PairElem
pPairElem = mkPairElem ("fst" *> lval) ("snd" *> lval)

-- RVal Parsers
rval :: Parser AST.RVal
rval = choice 
  [ AST.RExpr <$> expr
  , pArrLiter
  , pNewPair
  , AST.RPair <$> pPairElem
  , pCall
  ]

pArrLiter :: Parser AST.RVal
pArrLiter = mkArrLiter (L.brackets (expr `sepBy` ","))

pNewPair :: Parser AST.RVal
pNewPair = mkNewPair ("newpair" *> "(" *> expr) ("," *> expr <* ")")

pCall :: Parser AST.RVal
pCall = mkCall ("call" *> ident) (L.parens (expr `sepBy` ","))