{-# LANGUAGE OverloadedStrings #-}

module Expressions
  ( pInt,
    pBool,
    pChar,
    pString,
    pPairLit,
    pExpr,
    pArrayElem,
  )
where

import qualified AST 
import Control.Monad.Combinators.Expr 
import qualified Data.Text as T
import Parser (Parser, pToken, symbol, pIdent, brackets, parens, lexeme, keyword)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

pBool :: Parser AST.Expr
pBool = pToken $ AST.BoolLiter <$> ((True <$ keyword "true") <|> (False <$ keyword "false"))

pInt :: Parser AST.Expr
pInt = pToken $ AST.IntLiter <$> L.signed (return ()) L.decimal

pChar :: Parser AST.Expr
pChar = pToken $ AST.CharLiter <$> between (char '\'') (lexeme (char '\'')) L.charLiteral

pString :: Parser AST.Expr
pString = pToken $ AST.StrLiter . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pPairLit :: Parser AST.Expr
pPairLit = pToken $ AST.PairLiter <$ keyword "null"

pArrayElem :: Parser AST.ArrayElem
pArrayElem = pToken $ AST.ArrayElem <$> pIdent <*> some (brackets pExpr)

pTerm :: Parser AST.Expr
pTerm = choice 
  [ pInt,
    pBool,
    pChar,
    pString,
    pPairLit,
    AST.ArrayExpr <$> pArrayElem,
    AST.IdentExpr <$> pIdent,
    parens pExpr ]

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

binary :: T.Text -> (AST.Expr -> AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
binary s c = InfixL (c <$ symbol s)

prefix :: T.Text -> (AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
prefix s c = Prefix (c <$ symbol s)