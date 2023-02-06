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
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Parser (Parser, pToken, symbol, pIdent, brackets, parens, lexeme, keyword)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

pBool :: Parser AST.Expr
pBool = pToken $ AST.BoolLiter <$> ((True <$ keyword "true") <|> (False <$ keyword "false"))

pInt :: Parser AST.Expr
pInt = pToken $ AST.IntLiter <$> (L.signed (return ()) L.decimal >>= validWaccInt)
  where
    validWaccInt :: Integer -> Parser Integer
    validWaccInt x
      | x <= biggestWaccInt && x >= smallestWaccInt = pure x
      | otherwise = fail "Int literal outside of valid bounds!"

    biggestWaccInt = 2 ^ (31 :: Integer) 
    smallestWaccInt = -(2 ^ (31 :: Integer))

pChar :: Parser AST.Expr
pChar = pToken $ AST.CharLiter <$> between (char '\'') (lexeme (char '\'')) pChar'

pString :: Parser AST.Expr
pString = pToken $ AST.StrLiter . T.pack <$> (char '\"' *> many pChar' <* char '\"')

pChar' :: Parser Char
pChar' = satisfy validChar <|> (char '\\' *> satisfy escapedChar >>= toEscaped)
  where
    validChar :: Char -> Bool
    validChar c = ' ' <= c && c <= '\DEL' && notElem c ['\\', '\'', '"']

    escapedChar :: Char -> Bool
    escapedChar c = c `elem` ['0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\']

    toEscaped :: Char -> Parser Char
    toEscaped c = pure $ fromJust $ lookup c [ ('0', '\0')
                                             , ('b', '\b')
                                             , ('t', '\t')
                                             , ('n', '\n')
                                             , ('f', '\f')
                                             , ('r', '\r')
                                             , ('"', '\"')
                                             , ('\'', '\'')
                                             , ('\\', '\\') ]

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