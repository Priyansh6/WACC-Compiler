module StatementConstructors 
  ( mkDecAssign,
    mkAssign,
    mkRead,
    mkArrLiter,
    mkNewPair,
    mkPairElem,
    mkCall,
    mkFree,
    mkReturn,
    mkExit,
    mkIf,
    mkWhile
  ) 
where

import Parser (Parser, liftPos1, liftPos2, liftPos3)
import Text.Megaparsec ((<|>))

import qualified AST

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
mkPairElem l r = liftPos1 AST.Fst l <|> liftPos1 AST.Snd r

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