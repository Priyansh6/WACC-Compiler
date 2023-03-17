{-# LANGUAGE OverloadedStrings #-}

module Syntax.Repl
  ( repl,
    ReplInput (..),
    mkReplFunc,
    mkReplIdent,
    mkReplStat,
    mkReplWType
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
import Text.Megaparsec ((<?>), notFollowedBy)
import Text.Megaparsec.Char (string)

data ReplInput
  = ReplProgram AST.Program
  | ReplStat AST.Stat
  | ReplFunc AST.Func
  | ReplIdent AST.Ident
  | ReplWType AST.Ident

pDefaultIdents :: Parser AST.Ident
pDefaultIdents = choice $ map (mkIdent . string . T.pack) keywords

mkReplStat, mkReplFunc, mkReplIdent, mkReplProgram, mkReplWType :: Parser ReplInput
mkReplProgram = ReplProgram <$> program
mkReplStat = ReplStat <$> stat <* ";" <?> "statement"
mkReplFunc = ReplFunc <$> func <?> "function"
mkReplIdent = ReplIdent <$> (":" *> notFollowedBy ":" *> choice [ident, pDefaultIdents]) <?> "identifier"
mkReplWType = ReplWType <$> ("::" *> choice [ident, pDefaultIdents]) <?> "identifier"

repl :: Parser ReplInput
repl = mkReplProgram <|> mkReplFunc <|> mkReplStat <|> mkReplWType <|> mkReplIdent