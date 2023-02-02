{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Parser,
    pToken,
    symbol,
    pIdent,
    brackets,
    parens,
    lexeme
  )
where

import qualified AST 
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

pToken :: Parser a -> Parser a
pToken = lexeme . try

pIdent :: Parser AST.Ident
pIdent = pToken $ do 
  c <- char '_' <|> letterChar
  cs <- many (char '_' <|> alphaNumChar)
  return (AST.Ident (T.pack (c:cs)))

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- keyword :: String -> Parser ()
-- keyword k = token (string k *> notFollowedBy alphaNumChar)

keywords = ["begin", "end", "is", "end", "skip", "read",
            "free", "return", "exit", "print", "println",
            "if", "then", "else", "fi", "while", "do", 
            "done", "fst", "snd", "newpair", "call", 
            "int", "bool", "char", "string", "pair", 
            "len", "ord", "chr"]