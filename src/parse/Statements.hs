{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Statements 
  ( pStats
  , pStat
  , pWType
  , pSkip
  , pDecAssign
  , pAssign
  , pRead
  , pBaseType
  , pArrType
  , pPairType
  , pLVal
  , pRVal
  , pFree
  , pReturn
  , pCall
  , pPrint
  , pExit
  ) where

import Expressions (pExpr, pArrayElem, pIdent)
import Parser (Parser) 
import Text.Megaparsec
import qualified AST 
import qualified Lexer as L

pStats :: Parser AST.Stats
pStats = pStat `sepBy1` ";"

pStat :: Parser AST.Stat
pStat = choice 
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
pDecAssign = AST.DecAssign <$> pWType <*> pIdent <*> ("=" *> pRVal)

pAssign :: Parser AST.Stat
pAssign = AST.Assign <$> pLVal <*> ("=" *> pRVal)

pRead :: Parser AST.Stat
pRead = AST.Read <$> ("read" *> pLVal)

pWType :: Parser AST.WType
pWType = pArrType <|> pPairType <|> pBaseType 

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
    pPairElemType = pArrType <|> (AST.WUnit <$ "pair") <|> pBaseType 

pLVal :: Parser AST.LVal 
pLVal = (AST.LArray <$> pArrayElem) <|> (AST.LPair <$> pPairElem) <|> (AST.LIdent <$> pIdent)

pRVal :: Parser AST.RVal
pRVal = choice 
  [ AST.RExpr <$> pExpr
  , pArrLiter
  , pNewPair
  , AST.RPair <$> pPairElem
  , pCall
  ]

pArrLiter :: Parser AST.RVal
pArrLiter = AST.ArrayLiter <$> L.brackets (pExpr `sepBy` ",")

pNewPair :: Parser AST.RVal
pNewPair = AST.NewPair <$> ("newpair" *> "(" *> pExpr) <*> ("," *> pExpr <* ")")

pPairElem :: Parser AST.PairElem
pPairElem = (AST.Fst <$> ("fst" *> pLVal)) <|> (AST.Snd <$> ("snd" *> pLVal))

pCall :: Parser AST.RVal
pCall = AST.Call <$> ("call" *> pIdent) <*> L.parens pArgsList

pArgsList :: Parser [AST.Expr]
pArgsList = pExpr `sepBy` ","

pFree :: Parser AST.Stat
pFree = AST.Free <$> ("free" *> pExpr)

pReturn :: Parser AST.Stat
pReturn = AST.Return <$> ("return" *> pExpr)

pExit :: Parser AST.Stat
pExit = AST.Exit <$> ("exit" *> pExpr)

pPrint :: Parser AST.Stat
pPrint = AST.Print <$> ("print" *> pExpr)

pPrintln :: Parser AST.Stat
pPrintln = AST.Println <$> ("println" *> pExpr)

pIf :: Parser AST.Stat
pIf = AST.If <$> ("if" *> pExpr) <*> ("then" *> pStats) <*> ("else" *> pStats <* "fi")

pWhile :: Parser AST.Stat
pWhile = AST.While <$> ("while" *> pExpr) <*> ("do" *> pStats <* "done")

pBegin :: Parser AST.Stat
pBegin = AST.Begin <$> ("begin" *> pStats <* "end")