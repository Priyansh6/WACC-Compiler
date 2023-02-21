{-# LANGUAGE OverloadedStrings #-}

module Syntax.Expressions
  ( ident,
    arrayElem,
    expr
  )
where

import Control.Monad.Combinators.Expr 
import Text.Megaparsec
import Text.Megaparsec.Char (char)

import Syntax.ExpressionConstructors
import Syntax.Parser (Parser, deferLiftPos1, deferLiftPos2)
import qualified AST 
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
expr = mkExpr $ makeExprParser pTerm operatorTable 

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
binary s c = InfixL (c <* s <?> "binary operator")

prefix :: Parser () -> Parser (AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
prefix s c = Prefix (c <* s <?> "unary operator")