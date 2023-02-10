{-# LANGUAGE OverloadedStrings #-}

module Expressions
  ( ident,
    arrayElem,
    expr
  )
where

import qualified AST 
import Control.Applicative ((<**>))
import Control.Monad.Combinators.Expr 
import Parser (Parser, liftPos1, liftPos2, deferLiftPos1, deferLiftPos2, getPosition)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import qualified Data.Text as T
import qualified Lexer as L

ident :: Parser AST.Ident
ident = mkIdent L.ident

arrayElem :: Parser AST.ArrayElem
arrayElem = try $ mkArrayElem ident pArrayElemExprs

pBool :: Parser Bool
pBool = (True <$ "true") <|> (False <$ "false")

pChar :: Parser Char
pChar = try $ between (char '\'') "\'" L.char

pString :: Parser T.Text
pString = try $ T.pack <$> between (char '"') "\"" (many L.char)

pPairLiter :: Parser ()
pPairLiter = "null"

pArrayElemExprs :: Parser [AST.Expr]
pArrayElemExprs = try $ some $ L.brackets expr

pTerm :: Parser AST.Expr
pTerm = try $ choice 
  [ mkInt L.number,
    mkBool pBool,
    mkChar pChar,
    mkString pString,
    mkPairLiter pPairLiter,
    mkArrayExpr arrayElem,
    mkIdentExpr ident,
    L.parens expr ]

expr :: Parser AST.Expr
expr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser AST.Expr]]
operatorTable = 
  [ [ prefix "-"   (deferLiftPos1 AST.Neg)
    , prefix "len" (deferLiftPos1 AST.Len)
    , prefix "ord" (deferLiftPos1 AST.Ord)    
    , prefix "chr" (deferLiftPos1 AST.Chr)    
    , prefix "!"   (deferLiftPos1 AST.Not)    ]
  , [ binary "*"   (deferLiftPos2 (AST.:*:))  
    , binary "/"   (deferLiftPos2 (AST.:/:))  
    , binary "%"   (deferLiftPos2 (AST.:%:))  ]
  , [ binary "+"   (deferLiftPos2 (AST.:+:)) 
    , binary "-"   (deferLiftPos2 (AST.:-:))  ]
  , [ binary ">="  (deferLiftPos2 (AST.:>=:)) 
    , binary ">"   (deferLiftPos2 (AST.:>:))   
    , binary "<="  (deferLiftPos2 (AST.:<=:)) 
    , binary "<"   (deferLiftPos2 (AST.:<:))  ]
  , [ binary "=="  (deferLiftPos2 (AST.:==:)) 
    , binary "!="  (deferLiftPos2 (AST.:!=:)) ]
  , [ binary "&&"  (deferLiftPos2 (AST.:&&:)) ]
  , [ binary "||"  (deferLiftPos2 (AST.:||:)) ]]

binary :: Parser () -> Parser (AST.Expr -> AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
binary s c = InfixL (c <* s)

prefix :: Parser () -> Parser (AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
prefix s c = Prefix (c <* s)

-- Smart Constructors:
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