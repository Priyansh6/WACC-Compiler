module StatementConstructors 
  ( mkSkip,
    mkDecAssign,
    mkAssign,
    mkRead,
    mkArrLiter,
    mkNewPair,
    mkPairElem,
    mkCall,
    mkFree,
    mkReturn,
    mkExit,
    mkPrint,
    mkPrintln,
    mkIf,
    mkWhile,
    mkBegin
  ) 
where

import Parser (Parser, liftPos1, liftPos2, liftPos3, liftPosScopeIf, liftPosScopeWhile, liftScopeBegin)
import Text.Megaparsec ((<|>), (<?>), label)

import qualified AST

mkSkip :: Parser () -> Parser AST.Stat
mkSkip p = label "statement" $ AST.Skip <$ p

mkDecAssign :: Parser AST.WType -> Parser AST.Ident -> Parser AST.RVal -> Parser AST.Stat
mkDecAssign t i rval = label "variable declaration and assignment" $ liftPos3 AST.DecAssign t i rval

mkAssign :: Parser AST.LVal -> Parser AST.RVal -> Parser AST.Stat
mkAssign = (label "variable assignment" .) . liftPos2 AST.Assign 

mkRead :: Parser AST.LVal -> Parser AST.Stat
mkRead = label "statement" . liftPos1 AST.Read 

mkArrLiter :: Parser [AST.Expr] -> Parser AST.RVal 
mkArrLiter = label "array literal" . liftPos1 AST.ArrayLiter 

mkNewPair :: Parser AST.Expr -> Parser AST.Expr -> Parser AST.RVal
mkNewPair = (label "statement" .) . liftPos2 AST.NewPair 

mkPairElem :: Parser AST.LVal -> Parser AST.LVal -> Parser AST.PairElem
mkPairElem l r = label "statement" $ liftPos1 AST.Fst l <|> liftPos1 AST.Snd r

mkCall :: Parser AST.Ident -> Parser [AST.Expr] -> Parser AST.RVal
mkCall = (label "statement" .) . liftPos2 AST.Call 

mkFree :: Parser AST.Expr -> Parser AST.Stat
mkFree = label "statement" . liftPos1 AST.Free 

mkReturn :: Parser AST.Expr -> Parser AST.Stat 
mkReturn = label "statement" . liftPos1 AST.Return 

mkExit :: Parser AST.Expr -> Parser AST.Stat
mkExit = label "statement" . liftPos1 AST.Exit 

mkPrint :: Parser AST.Expr -> Parser AST.Stat
mkPrint e = label "statement" $ AST.Print <$> e

mkPrintln :: Parser AST.Expr -> Parser AST.Stat
mkPrintln e = label "statement" $ AST.Println <$> e

mkIf :: Parser AST.Expr -> Parser AST.Stats -> Parser AST.Stats -> Parser AST.Stat
mkIf c b1 b2 = label "statement" $ liftPosScopeIf AST.If (c <?> "condition") (b1 <?> "branch") (b2 <?> "branch")

mkWhile :: Parser AST.Expr -> Parser AST.Stats -> Parser AST.Stat
mkWhile c b = label "statement" $ liftPosScopeWhile AST.While (c <?> "condition") (b <?> "body")

mkBegin :: Parser AST.Stats -> Parser AST.Stat
mkBegin = label "statement" . liftScopeBegin AST.Begin