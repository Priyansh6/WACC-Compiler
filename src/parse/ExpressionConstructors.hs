module ExpressionConstructors
  ( mkIdent,
    mkArrayElem,
    mkIdentExpr,
    mkArrayExpr,
    mkBool,
    mkInt,
    mkChar,
    mkString,
    mkPairLiter
   ) 
where

import Control.Applicative ((<**>))
import Parser (Parser, liftPos1, liftPos2, deferLiftPos1, deferLiftPos2, getPosition)

import qualified AST
import qualified Data.Text as T

mkIdent :: Parser T.Text -> Parser AST.Ident
mkIdent = liftPos1 AST.Ident 

mkArrayElem :: Parser AST.Ident -> Parser [AST.Expr] -> Parser AST.ArrayElem
mkArrayElem = liftPos2 AST.ArrayElem 

mkIdentExpr :: Parser AST.Ident -> Parser AST.Expr
mkIdentExpr = liftPos1 AST.IdentExpr 

mkArrayExpr :: Parser AST.ArrayElem -> Parser AST.Expr
mkArrayExpr = liftPos1 AST.ArrayExpr 

mkBool :: Parser Bool -> Parser AST.Expr
mkBool = liftPos1 AST.BoolLiter

mkInt :: Parser Integer -> Parser AST.Expr
mkInt = liftPos1 AST.IntLiter 

mkChar :: Parser Char -> Parser AST.Expr
mkChar = liftPos1 AST.CharLiter 

mkString :: Parser T.Text -> Parser AST.Expr
mkString = liftPos1 AST.StrLiter 

mkPairLiter :: Parser () -> Parser AST.Expr
mkPairLiter p = getPosition <**> (AST.PairLiter <$ p)