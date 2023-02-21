{-# LANGUAGE OverloadedStrings #-}

module Semantic.Type.CheckStatements (checkStats) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Writer ()
import qualified Data.Map as M

import AST
import Semantic.Type.CheckExpressions
import Semantic.Type.CheckRLVals
import Semantic.Type.SymbolTable
import Semantic.Errors

checkStats :: Stats -> ScopedSemanticAnalyser ()
checkStats = mapM_ checkStat

checkStat :: Stat -> ScopedSemanticAnalyser ()
checkStat (DecAssign ltype ident rval pos) = do
  insertAssign ltype ident
  rtype <- checkRVal rval
  _ <- areTypesCompatible ltype rtype pos
  return ()
checkStat (Assign lval rval pos) = do
  ltype <- checkLVal lval
  rtype <- checkRVal rval
  _ <- areTypesCompatible ltype rtype pos
  return ()
checkStat (Read lval pos) = do
  wtype <- checkLVal lval
  case wtype of
    WInt -> return ()
    WChar -> return ()
    _ -> void $ throwError $ IncompatibleTypes pos [WInt, WChar] wtype
checkStat (Free expr pos) = do
  wtype <- checkExprType expr
  case wtype of
    WPair _ _ -> return ()
    WArr _ _ -> return ()
    _ -> throwError $ IncompatibleTypes pos [pairErrorType, arrayErrorType] wtype
checkStat (Return expr pos) = do
  rtype <- checkExprType expr
  ftype <- ask
  _ <- case ftype of
    Nothing -> throwError $ IllegalReturn pos
    Just wtype -> areTypesCompatible wtype rtype pos
  return ()
checkStat (Exit expr pos) = do
  wtype <- checkExprType expr
  unless (wtype == WInt) $ throwError $ IncompatibleTypes pos [WInt] wtype
checkStat (Print expr) = void $ checkExprType expr
checkStat (Println expr) = void $ checkExprType expr
checkStat (If expr stats1 _ stats2 _ pos) = do
  checkStats stats1
  checkStats stats2
  wtype <- checkExprType expr
  unless (wtype == WBool) $ throwError $ IncompatibleTypes pos [WBool] wtype
checkStat (While expr stats _ pos) = do
  checkStats stats
  wtype <- checkExprType expr
  unless (wtype == WBool) $ throwError $ IncompatibleTypes pos [WBool] wtype
checkStat (Begin stats _) = checkStats stats
checkStat Skip = return ()

insertAssign :: WType -> Ident -> ScopedSemanticAnalyser ()
insertAssign (WArr wtype x) ident = modify $ M.insert ident (ArrType wtype x)
insertAssign (WPair wtype wtype') ident = modify $ M.insert ident (PairType wtype wtype')
insertAssign wtype ident = modify $ M.insert ident (VarType wtype)