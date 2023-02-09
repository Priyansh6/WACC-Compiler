{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Statements 
  ( pStats
  , pStat
  , pWType
  , pSkip
  , mkDecAssign
  , mkAssign
  , mkRead
  , pBaseType
  , pArrType
  , pPairType
  , pLVal
  , pRVal
  , mkFree
  , mkReturn
  , mkCall
  , pPrint
  , mkExit
  ) where

import Expressions (pExpr, mkArrayElem, mkIdent)
import Parser (Parser, liftPos1, liftPos2, liftPos3) 
import Text.Megaparsec
import qualified AST 
import qualified Lexer as L

pStats :: Parser AST.Stats
pStats = pStat `sepBy1` ";"

pStat :: Parser AST.Stat
pStat = choice 
  [
    pSkip,
    mkDecAssign,
    mkAssign,
    mkRead,
    mkFree,
    mkReturn,
    mkExit,
    pPrint,
    pPrintln,
    mkIf,
    pWhile,
    pBegin
  ]

pSkip :: Parser AST.Stat
pSkip = AST.Skip <$ "skip"

mkDecAssign :: Parser AST.Stat
mkDecAssign = liftPos3 AST.DecAssign pWType mkIdent ("=" *> pRVal)

mkAssign :: Parser AST.Stat
mkAssign = liftPos2 AST.Assign pLVal ("=" *> pRVal)

mkRead :: Parser AST.Stat
mkRead = liftPos1 AST.Read ("read" *> pLVal)

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
pLVal = (AST.LArray <$> mkArrayElem) <|> (AST.LPair <$> pPairElem) <|> (AST.LIdent <$> mkIdent)

pRVal :: Parser AST.RVal
pRVal = choice 
  [ AST.RExpr <$> pExpr
  , mkArrLiter
  , mkNewPair
  , AST.RPair <$> pPairElem
  , mkCall
  ]

pArrLiter :: Parser [AST.Expr]
pArrLiter = L.brackets (pExpr `sepBy` ",")

mkArrLiter :: Parser AST.RVal
mkArrLiter = liftPos1 AST.ArrayLiter pArrLiter

pNewPairFstExpr :: Parser AST.Expr
pNewPairFstExpr = "newpair" *> "(" *> pExpr

pNewPairSndExpr :: Parser AST.Expr
pNewPairSndExpr = "," *> pExpr <* ")"

mkNewPair :: Parser AST.RVal
mkNewPair = liftPos2 AST.NewPair pNewPairFstExpr pNewPairSndExpr

pPairElem :: Parser AST.PairElem
pPairElem = mkPairFst <|> mkPairSnd

mkPairFst :: Parser AST.PairElem
mkPairFst = liftPos1 AST.Fst ("fst" *> pLVal)

mkPairSnd :: Parser AST.PairElem
mkPairSnd = liftPos1 AST.Snd ("snd" *> pLVal)

mkCall :: Parser AST.RVal
mkCall = liftPos2 AST.Call ("call" *> mkIdent) (L.parens pArgsList)

pArgsList :: Parser [AST.Expr]
pArgsList = pExpr `sepBy` ","

pFree :: Parser AST.Expr
pFree = "free" *> pExpr

mkFree :: Parser AST.Stat
mkFree = liftPos1 AST.Free pFree

pReturn :: Parser AST.Expr
pReturn = "return" *> pExpr

mkReturn :: Parser AST.Stat
mkReturn = liftPos1 AST.Return pReturn

pExit :: Parser AST.Expr
pExit = "exit" *> pExpr

mkExit :: Parser AST.Stat
mkExit = liftPos1 AST.Exit pExit

pPrint :: Parser AST.Stat
pPrint = AST.Print <$> ("print" *> pExpr)

pPrintln :: Parser AST.Stat
pPrintln = AST.Println <$> ("println" *> pExpr)

pIfExpr :: Parser AST.Expr
pIfExpr = "if" *> pExpr

pIfBranchA :: Parser AST.Stats
pIfBranchA = "then" *> pStats

pIfBranchB :: Parser AST.Stats
pIfBranchB = "else" *> pStats <* "fi"

mkIf :: Parser AST.Stat
mkIf = liftPos3 AST.If pIfExpr pIfBranchA pIfBranchB

pWhileExpr :: Parser AST.Expr
pWhileExpr = "while" *> pExpr

pWhileBody :: Parser AST.Stats
pWhileBody = "do" *> pStats <* "done"

pWhile :: Parser AST.Stat
pWhile = liftPos2 AST.While pWhileExpr pWhileBody

pBegin :: Parser AST.Stat
pBegin = AST.Begin <$> ("begin" *> pStats <* "end")