{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Statements 
  ( wtype
  , pBaseType
  , pArrType
  , pPairType
  , stats
  , stat
  , pSkip
  , pDecAssign
  , pAssign
  , pRead
  , lval
  , rval
  , pFree
  , pReturn
  , pCall
  , pPrint
  , pExit
  ) where

import Expressions (expr, arrayElem, ident)
import Parser (Parser, liftPos1, liftPos2, liftPos3) 
import Text.Megaparsec
import qualified AST 
import qualified Lexer as L

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
pSkip = AST.Skip <$ "skip"

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
pPrint = AST.Print <$> ("print" *> expr)

pPrintln :: Parser AST.Stat
pPrintln = AST.Println <$> ("println" *> expr)

pIf :: Parser AST.Stat
pIf = mkIf ("if" *> expr) ("then" *> stats) ("else" *> stats <* "fi")

pWhile :: Parser AST.Stat
pWhile = mkWhile ("while" *> expr) ("do" *> stats <* "done")

pBegin :: Parser AST.Stat
pBegin = AST.Begin <$> ("begin" *> stats <* "end")

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

-- Smart Constructors:
mkDecAssign :: Parser AST.WType -> Parser AST.Ident -> Parser AST.RVal -> Parser AST.Stat
mkDecAssign = liftPos3 AST.DecAssign 

mkAssign :: Parser AST.LVal -> Parser AST.RVal -> Parser AST.Stat
mkAssign = liftPos2 AST.Assign 

mkRead :: Parser AST.LVal -> Parser AST.Stat
mkRead = liftPos1 AST.Read 

mkArrLiter :: Parser [AST.Expr] -> Parser AST.RVal 
mkArrLiter = liftPos1 AST.ArrayLiter 

mkNewPair :: Parser AST.Expr -> Parser AST.Expr -> Parser AST.RVal
mkNewPair = liftPos2 AST.NewPair 

mkPairElem :: Parser AST.LVal -> Parser AST.LVal -> Parser AST.PairElem
mkPairElem l r = liftPos1 AST.Fst l <|> liftPos1 AST.Fst r

mkCall :: Parser AST.Ident -> Parser [AST.Expr] -> Parser AST.RVal
mkCall = liftPos2 AST.Call 

mkFree :: Parser AST.Expr -> Parser AST.Stat
mkFree = liftPos1 AST.Free 

mkReturn :: Parser AST.Expr -> Parser AST.Stat 
mkReturn = liftPos1 AST.Return 

mkExit :: Parser AST.Expr -> Parser AST.Stat
mkExit = liftPos1 AST.Exit 

mkIf :: Parser AST.Expr -> Parser AST.Stats -> Parser AST.Stats -> Parser AST.Stat
mkIf = liftPos3 AST.If 

mkWhile :: Parser AST.Expr -> Parser AST.Stats -> Parser AST.Stat
mkWhile = liftPos2 AST.While