{-# LANGUAGE FlexibleInstances, GADTs, OverloadedStrings, TypeSynonymInstances#-}

module Lexer (ident, number, char, brackets, parens, fully) where

import Data.Maybe
import Data.String
import Parser (Parser)
import Text.Megaparsec hiding (token)

import qualified Data.Text as T
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

token :: Parser a -> Parser a
token = lexeme . try

keyword :: T.Text -> Parser ()
keyword k = token (C.string k *> notFollowedBy C.alphaNumChar)

keywords :: [String]
keywords = ["begin", "end", "is", "end", "skip", "read",
            "free", "return", "exit", "print", "println",
            "if", "then", "else", "fi", "while", "do", 
            "done", "fst", "snd", "null", "newpair", "call", 
            "int", "bool", "char", "string", "pair", 
            "len", "ord", "chr"]

instance {-# OVERLAPPING #-} u~() => IsString (Parser u) where
  fromString s
    | s `elem` keywords = keyword (T.pack s)
    | otherwise = () <$ token (C.string (T.pack s))

ident :: Parser T.Text
ident = token $ do 
  c <- C.char '_' <|> C.letterChar
  cs <- many (C.char '_' <|> C.alphaNumChar)
  if (c:cs) `elem` keywords 
    then fail "ident is a keyword!"
    else return (T.pack (c:cs))

number :: Parser Integer
number 
  = token $ L.signed (return ()) L.decimal >>= validWACCInteger
  where
    validWACCInteger :: Integer -> Parser Integer
    validWACCInteger x
      | x <= biggestWaccInt && x >= smallestWaccInt = pure x
      | otherwise = fail "Int literal outside of valid bounds!"

    biggestWaccInt = 2 ^ (31 :: Integer) 
    smallestWaccInt = -(2 ^ (31 :: Integer))

char :: Parser Char
char
  = try $ satisfy validChar <|> (C.char '\\' *> satisfy escapedChar >>= toEscaped)
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

brackets :: Parser a -> Parser a
brackets = try . between "[" "]"

parens :: Parser a -> Parser a
parens = try . between "(" ")"

fully :: Parser a -> Parser a
fully p = sc *> p <* eof