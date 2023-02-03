{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Statements (module Statements) where

import qualified AST 
import Control.Monad.Combinators.Expr 
import qualified Data.Text as T
import Parser (Parser, pToken, symbol, pIdent, brackets, parens, lexeme, keyword)
import Expressions (pExpr, pArrayElem)
import Text.Megaparsec
import Text.Megaparsec.Char

pStats :: Parser AST.Stats
pStats = pStat `sepBy1` symbol ";"

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
pSkip = AST.Skip <$ keyword "skip"

pDecAssign :: Parser AST.Stat
pDecAssign = AST.DecAssign <$> pWType <*> pIdent <*> (symbol "=" *> pRVal)

pAssign :: Parser AST.Stat
pAssign = AST.Assign <$> pLVal <*> (symbol "=" *> pRVal)

pRead :: Parser AST.Stat
pRead = AST.Read <$> (keyword "read" *> pLVal)

pWType :: Parser AST.WType
pWType = pArrType <|> pPairType <|> pBaseType 

pBaseType :: Parser AST.WType 
pBaseType = try $ choice 
  [ AST.WInt <$ keyword "int"
  , AST.WBool <$ keyword "bool"
  , AST.WChar <$ keyword "char"
  , AST.WStr <$ keyword "string" ]

pArrType :: Parser AST.WType
pArrType = try $ do
            t <- pBaseType <|> pPairType
            bs <- some (symbol "[]")
            let dimension = length bs
            return (AST.WArr t dimension)

pPairType :: Parser AST.WType
pPairType = AST.WPair <$> (keyword "pair" *> symbol "(" *> pPairElemType) <*> (symbol "," *> pPairElemType <* symbol ")")
  where
    pPairElemType :: Parser AST.WType
    pPairElemType = pArrType <|> (AST.WUnit <$ keyword "pair") <|> pBaseType 

pLVal :: Parser AST.LVal 
pLVal = (AST.LIdent <$> pIdent) <|> (AST.LArray <$> pArrayElem) <|> (AST.LPair <$> pPairElem)

pRVal :: Parser AST.RVal
pRVal = choice 
  [ AST.RExpr <$> pExpr
  , pArrLiter
  , pNewPair
  , AST.RPair <$> pPairElem
  , pCall
  ]

pArrLiter :: Parser AST.RVal
pArrLiter = AST.ArrayLiter <$> brackets (pExpr `sepBy1` symbol ",")

pNewPair :: Parser AST.RVal
pNewPair = AST.NewPair <$> (keyword "newpair" *> symbol "(" *> pExpr) <*> (symbol "," *> pExpr <* symbol ")")

pPairElem :: Parser AST.PairElem
pPairElem = (AST.Fst <$> (keyword "fst" *> pLVal)) <|> (AST.Snd <$> (keyword "snd" *> pLVal))

pCall :: Parser AST.RVal
pCall = AST.Call <$> (keyword "call" *> pIdent) <*> brackets pArgsList

pArgsList :: Parser [AST.Expr]
pArgsList = pExpr `sepBy` symbol ","

pFree :: Parser AST.Stat
pFree = AST.Free <$> (keyword "free" *> pExpr)

pReturn :: Parser AST.Stat
pReturn = AST.Return <$> (keyword "return" *> pExpr)

pExit :: Parser AST.Stat
pExit = AST.Exit <$> (keyword "exit" *> pExpr)

pPrint :: Parser AST.Stat
pPrint = AST.Print <$> (keyword "print" *> pExpr)

pPrintln :: Parser AST.Stat
pPrintln = AST.Println <$> (keyword "println" *> pExpr)

pIf :: Parser AST.Stat
pIf = AST.If <$> (keyword "if" *> pExpr) <*> (keyword "then" *> pStats) <*> (keyword "else" *> pStats <* keyword "fi")

pWhile :: Parser AST.Stat
pWhile = AST.While <$> (keyword "while" *> pExpr) <*> (keyword "do" *> pStats <* keyword "done")

pBegin :: Parser AST.Stat
pBegin = AST.Begin <$> (keyword "begin" *> pStats <* keyword "end")