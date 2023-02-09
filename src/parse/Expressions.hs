{-# LANGUAGE OverloadedStrings #-}

module Expressions
  ( mkIdent,
    pExpr,
    mkArrayElem,
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

mkIdent :: Parser AST.Ident
mkIdent = liftPos1 AST.Ident L.ident 

mkArrayElem :: Parser AST.ArrayElem
mkArrayElem = try $ liftPos2 AST.ArrayElem mkIdent (some (L.brackets pExpr))

pBool :: Parser Bool
pBool = (True <$ "true") <|> (False <$ "false")

pChar :: Parser Char
pChar = try $ between (char '\'') "\'" L.char

pString :: Parser T.Text
pString = try $ T.pack <$> between (char '"') "\"" (many L.char)

mkIdentExpr :: Parser AST.Expr
mkIdentExpr = liftPos1 AST.IdentExpr mkIdent 

mkArrayExpr :: Parser AST.Expr
mkArrayExpr = liftPos1 AST.ArrayExpr mkArrayElem 

mkBool :: Parser AST.Expr
mkBool = liftPos1 AST.BoolLiter pBool 

mkInt :: Parser AST.Expr
mkInt = liftPos1 AST.IntLiter L.number 

mkChar :: Parser AST.Expr
mkChar = liftPos1 AST.CharLiter pChar 

mkString :: Parser AST.Expr
mkString = liftPos1 AST.StrLiter pString 

mkPairLit :: Parser AST.Expr
mkPairLit = getPosition <**> (AST.PairLiter <$ "null")

pTerm :: Parser AST.Expr
pTerm = try $ choice 
  [ mkInt,
    mkBool,
    mkChar,
    mkString,
    mkPairLit,
    mkArrayExpr,
    mkIdentExpr,
    L.parens pExpr ]

pExpr :: Parser AST.Expr
pExpr = makeExprParser pTerm operatorTable

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