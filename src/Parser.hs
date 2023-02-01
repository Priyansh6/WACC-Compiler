{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Parser,
    pInt,
    pBool,
    pChar,
    pString,
    pPairLit,
    pIdent,
    pArrayElem,
    pExpr,
  )
where

import qualified AST 
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr 

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

pToken :: Parser a -> Parser a
pToken = lexeme . try

pBool :: Parser AST.Expr
pBool = pToken $ AST.BoolLiter <$> ((True <$ string "true") <|> (False <$ string "false"))

pInt :: Parser AST.Expr
pInt = pToken $ AST.IntLiter <$> L.signed (return ()) L.decimal

pChar :: Parser AST.Expr
pChar = pToken $ AST.CharLiter <$> between (char '\'') (lexeme (char '\'')) L.charLiteral

pString :: Parser AST.Expr
pString = pToken $ AST.StrLiter . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pPairLit :: Parser AST.Expr
pPairLit = pToken $ AST.PairLiter <$ string "null"

pIdent :: Parser AST.Ident
pIdent = pToken $ do 
  c <- char '_' <|> letterChar
  cs <- many (char '_' <|> alphaNumChar)
  return (AST.Ident (T.pack (c:cs)))

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pArrayElem :: Parser AST.ArrayElem
pArrayElem = pToken $ do
  ident <- pIdent
  exprs <- many (brackets pExpr)
  return (AST.ArrayElem ident exprs)

pTerm :: Parser AST.Expr
pTerm = choice 
  [ pInt,
    pBool,
    pChar,
    pString,
    pPairLit,
    AST.IdentExpr <$> pIdent,
    AST.ArrayExpr <$> pArrayElem,
    parens pExpr ]

pExpr :: Parser AST.Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser AST.Expr]]
operatorTable = 
  [ [ prefix "-" AST.Neg ]
  , [ binary "*" (AST.:*:)
    , binary "/" (AST.:/:)
    , binary "%" (AST.:%:) ]
  , [ binary "+" (AST.:+:)
    , binary "-" (AST.:-:) ]
  , [ binary ">" (AST.:>:) 
    , binary ">=" (AST.:>=:)
    , binary "<=" (AST.:<=:)
    , binary "<=" (AST.:<=:) ]
  , [ binary "==" (AST.:==:)
    , binary "!=" (AST.:!=:) ]
  , [ binary "&&" (AST.:&&:) ]
  , [ binary "||" (AST.:||:) ]]

binary :: T.Text -> (AST.Expr -> AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
binary s c = InfixL (c <$ symbol s)

prefix :: T.Text -> (AST.Expr -> AST.Expr) -> Operator Parser AST.Expr
prefix s c = Prefix (c <$ symbol s)