{-# LANGUAGE OverloadedStrings #-}

module CheckTypes (checkProg) where

import AST
import Control.Monad.Reader
import Control.Monad.Trans.Writer ()
import Control.Monad.State
import qualified Data.Map as M
import CheckStatements 
import SymbolTable

checkProg :: Program -> SemanticAnalyser ()
checkProg (Program funcs stats) = addFuncsToSymbolTable funcs >> checkFuncs funcs >> runReaderT (checkStats stats) Nothing

checkFuncs :: [Func] -> SemanticAnalyser ()
checkFuncs = mapM_ checkFunc

addFuncsToSymbolTable :: [Func] -> SemanticAnalyser ()
addFuncsToSymbolTable = mapM_ addFuncToSymbolTable

addFuncToSymbolTable :: Func -> SemanticAnalyser ()
addFuncToSymbolTable (Func wtype ident params _ _ _) = do
  let (_, paramIds) = unzip params
  modify (M.insert ident (FuncType wtype paramIds))
  insertParams params

checkFunc :: Func -> SemanticAnalyser ()
checkFunc (Func wtype _ _ stats _ _) = runReaderT (checkStats stats) (Just wtype)

insertParams :: [(WType, Ident)] -> SemanticAnalyser ()
insertParams = mapM_ (uncurry insertParam)

insertParam :: WType -> Ident -> SemanticAnalyser ()
insertParam wtype ident = modify $ M.insert ident (VarType wtype)
