{-# LANGUAGE OverloadedStrings #-}

module Syntax.Repl
  ( repl,
    ReplInput (..),
    mkReplFunc,
    mkReplIdent,
    mkReplStat,
  )
where

import qualified AST
import Control.Monad.Combinators
import qualified Data.Text as T
import Lexer (keywords)
import Syntax.ExpressionConstructors (mkIdent)
import Syntax.Expressions (ident)
import Syntax.Parser (Parser)
import Syntax.Program (func, program)
import Syntax.Statements (stat)
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char (string)

data ReplInput = ReplIdent AST.Ident | ReplStat AST.Stat | ReplFunc AST.Func | ReplProgram AST.Program

pDefaultIdents :: Parser AST.Ident
pDefaultIdents = choice $ map (mkIdent . string . T.pack) keywords

mkReplStat, mkReplFunc, mkReplIdent, mkReplProgram :: Parser ReplInput
mkReplProgram = ReplProgram <$> program
mkReplStat = ReplStat <$> stat <* ";" <?> "statement"
mkReplFunc = ReplFunc <$> func <?> "function"
mkReplIdent = ReplIdent <$> (":" *> choice [ident, pDefaultIdents]) <?> "identifier"

repl :: Parser ReplInput
repl = mkReplProgram <|> mkReplFunc <|> mkReplStat <|> mkReplIdent