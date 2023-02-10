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
import Parser (Parser, liftPos1, liftPos2, getPosition)
import Text.Megaparsec (label)

import qualified AST
import qualified Data.Text as T

mkIdent :: Parser T.Text -> Parser AST.Ident
mkIdent = label "identifier" . liftPos1 AST.Ident 

mkArrayElem :: Parser AST.Ident -> Parser [AST.Expr] -> Parser AST.ArrayElem
mkArrayElem = (label "array expression" .) . liftPos2 AST.ArrayElem

mkIdentExpr :: Parser AST.Ident -> Parser AST.Expr
mkIdentExpr = label "identifier" . liftPos1 AST.IdentExpr 

mkArrayExpr :: Parser AST.ArrayElem -> Parser AST.Expr
mkArrayExpr = label "array expression" . liftPos1 AST.ArrayExpr 

mkBool :: Parser Bool -> Parser AST.Expr
mkBool = label "boolean literal" . liftPos1 AST.BoolLiter

mkInt :: Parser Integer -> Parser AST.Expr
mkInt = label "int literal" . liftPos1 AST.IntLiter 

mkChar :: Parser Char -> Parser AST.Expr
mkChar = label "char literal" . liftPos1 AST.CharLiter 

mkString :: Parser T.Text -> Parser AST.Expr
mkString = label "string literal" . liftPos1 AST.StrLiter 

mkPairLiter :: Parser () -> Parser AST.Expr
mkPairLiter p = label "null" getPosition <**> (AST.PairLiter <$ p)