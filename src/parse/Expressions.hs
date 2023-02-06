{-# LANGUAGE OverloadedStrings #-}

module Expressions
  ( pIdent,
    pBool,
    pInt,
    pChar,
    pString,
    pPairLit,
    pArrayElem,
    pExpr,
  )
where

import qualified AST 
import Control.Monad.Combinators.Expr 
import qualified Data.Text as T
import Parser (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import qualified Lexer as L

pIdent :: Parser AST.Ident
pIdent = AST.Ident <$> L.ident

pBool :: Parser AST.Expr
pBool = AST.BoolLiter <$> ((True <$ "true") <|> (False <$ "false"))

pInt :: Parser AST.Expr
pInt = AST.IntLiter <$> L.number

pChar :: Parser AST.Expr
pChar = try $ AST.CharLiter <$> between (char '\'') "\'" L.char

pString :: Parser AST.Expr
pString = try $ AST.StrLiter . T.pack <$> between (char '"') "\"" (many L.char)

pPairLit :: Parser AST.Expr
pPairLit = try $ AST.PairLiter <$ "null"

pArrayElem :: Parser AST.ArrayElem
pArrayElem = try $ AST.ArrayElem <$> pIdent <*> some (L.brackets pExpr)

pTerm :: Parser AST.Expr
pTerm = try $ choice 
  [ pInt,
    pBool,
    pChar,
    pString,
    pPairLit,
    AST.ArrayExpr <$> pArrayElem,
    AST.IdentExpr <$> pIdent,
    L.parens pExpr ]

pExpr :: Parser AST.Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser AST.Expr]]
operatorTable = 
  [ [ prefix "-" AST.Neg 
    , prefix "len" AST.Len
    , prefix "ord" AST.Ord
    , prefix "chr" AST.Chr 
    , prefix "!" AST.Not ]
  , [ binary "*" (AST.:*:)
    , binary "/" (AST.:/:)
    , binary "%" (AST.:%:) ]
  , [ binary "+" (AST.:+:)
    , binary "-" (AST.:-:) ]
  , [ binary ">=" (AST.:>=:)
    , binary ">" (AST.:>:) 
    , binary "<=" (AST.:<=:)
    , binary "<" (AST.:<:) ]
  , [ binary "==" (AST.:==:)
    , binary "!=" (AST.:!=:) ]
  , [ binary "&&" (AST.:&&:) ]
  , [ binary "||" (AST.:||:) ]]

binary :: Parser () -> (AST.Expr -> AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
binary s c = InfixL (c <$ s)

prefix :: Parser () -> (AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
prefix s c = Prefix (c <$ s)