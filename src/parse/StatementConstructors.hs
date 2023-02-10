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
import Text.Megaparsec ((<|>), (<?>), label)

import qualified AST

mkDecAssign :: Parser AST.WType -> Parser AST.Ident -> Parser AST.RVal -> Parser AST.Stat
mkDecAssign t i rval = label "variable declaration and assignment" $ liftPos3 AST.DecAssign t i rval

mkAssign :: Parser AST.LVal -> Parser AST.RVal -> Parser AST.Stat
mkAssign = (label "variable assignment" .) . liftPos2 AST.Assign 

mkRead :: Parser AST.LVal -> Parser AST.Stat
mkRead = label "read statement" . liftPos1 AST.Read 

mkArrLiter :: Parser [AST.Expr] -> Parser AST.RVal 
mkArrLiter = label "array literal" . liftPos1 AST.ArrayLiter 

mkNewPair :: Parser AST.Expr -> Parser AST.Expr -> Parser AST.RVal
mkNewPair = (label "newpair statement" .) . liftPos2 AST.NewPair 

mkPairElem :: Parser AST.LVal -> Parser AST.LVal -> Parser AST.PairElem
mkPairElem l r = label "fst or snd statement" $ liftPos1 AST.Fst l <|> liftPos1 AST.Snd r

mkCall :: Parser AST.Ident -> Parser [AST.Expr] -> Parser AST.RVal
mkCall = (label "call statement" .) . liftPos2 AST.Call 

mkFree :: Parser AST.Expr -> Parser AST.Stat
mkFree = label "free statement" . liftPos1 AST.Free 

mkReturn :: Parser AST.Expr -> Parser AST.Stat 
mkReturn = label "return statement" . liftPos1 AST.Return 

mkExit :: Parser AST.Expr -> Parser AST.Stat
mkExit = label "exit statement" . liftPos1 AST.Exit 

mkIf :: Parser AST.Expr -> Parser AST.Stats -> Parser AST.Stats -> Parser AST.Stat
mkIf c b1 b2 = label "if statement" $ liftPos3 AST.If (c <?> "condition") (b1 <?> "branch") (b2 <?> "branch")

mkWhile :: Parser AST.Expr -> Parser AST.Stats -> Parser AST.Stat
mkWhile c b = label "while statement" $ liftPos2 AST.While (c <?> "condition") (b <?> "body")