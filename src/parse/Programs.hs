{-# LANGUAGE OverloadedStrings #-}

module Programs 
  ( pProgram,
    pFunc
  ) 
where

import Control.Monad.Combinators
import Expressions (pIdent) 
import Parser (Parser)
import Text.Megaparsec
import Statements (pStats, pWType)
import qualified AST 
import qualified Lexer as L

pProgram :: Parser AST.Program
pProgram = AST.Program <$> ("begin" *> many pFunc) <*> (pStats <* "end")

pFunc :: Parser AST.Func
pFunc = try $ AST.Func <$> pWType <*> pIdent <*> L.parens pParamList <*> ("is" *> pStats <* "end") >>= vFunc
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
pParamList = pParam `sepBy` ","
  where
    pParam :: Parser (AST.WType, AST.Ident)
    pParam = (,) <$> pWType <*> pIdent