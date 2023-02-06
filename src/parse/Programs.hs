{-# LANGUAGE OverloadedStrings #-}

module Programs 
  ( pProgram,
    pFunc
  ) 
where

import qualified Data.Text as T
import qualified AST 
import Control.Monad.Combinators.Expr 
import qualified Data.Text as T
import Parser (Parser, pToken, symbol, pIdent, brackets, parens, lexeme, keyword)
import Expressions (pExpr, pArrayElem)
import Statements
import Text.Megaparsec
import Text.Megaparsec.Char

pProgram :: Parser AST.Program
pProgram = AST.Program <$> (keyword "begin" *> many pFunc) <*> (pStats <* keyword "end")

pFunc :: Parser AST.Func
pFunc = try $ AST.Func <$> pWType <*> pIdent <*> parens pParamList <*> (keyword "is" *> pStats <* keyword "end") >>= vFunc
  where
    vFunc :: AST.Func -> Parser AST.Func
    vFunc f@(AST.Func _ _ _ xs)
      | validThroughAllPaths (last xs) = pure f
      | otherwise = fail "All paths through function must end with either a return or exit statement!"

    validThroughAllPaths :: AST.Stat -> Bool
    validThroughAllPaths (AST.Return _) = True
    validThroughAllPaths (AST.Exit _) = True
    validThroughAllPaths (AST.If _ xs xs') = validThroughAllPaths (last xs) && validThroughAllPaths (last xs')
    validThroughAllPaths (AST.While _ xs) = validThroughAllPaths (last xs)
    validThroughAllPaths (AST.Begin xs) = validThroughAllPaths (last xs)
    validThroughAllPaths _ = False

pParamList :: Parser [(AST.WType, AST.Ident)]
pParamList = pParam `sepBy` symbol ","
  where
    pParam :: Parser (AST.WType, AST.Ident)
    pParam = (,) <$> pWType <*> pIdent