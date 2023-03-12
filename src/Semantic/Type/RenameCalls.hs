module Semantic.Type.RenameCalls (renameCallsProg) where

import AST
import Semantic.Rename.Utils (addTypesToFuncIdent)
import Semantic.Type.CheckExpressions (checkExprType)
import Semantic.Type.SymbolTable

renameCallsProg :: Program -> ScopedSemanticAnalyser Program
renameCallsProg (Program fs ss) = Program <$> mapM renameCallsFunc fs <*> mapM renameCallsStat ss

renameCallsFunc :: Func -> ScopedSemanticAnalyser Func
renameCallsFunc (Func t name ps ss s pos) = Func t name ps <$> mapM renameCallsStat ss <*> return s <*> return pos

renameCallsStat :: Stat -> ScopedSemanticAnalyser Stat
renameCallsStat (DecAssign t i r pos) = DecAssign t i <$> renameCallsRVal r <*> return pos
renameCallsStat (Assign l r pos) = Assign l <$> renameCallsRVal r <*> return pos
renameCallsStat (If e ss1 s1 ss2 s2 pos) = If e <$> mapM renameCallsStat ss1 <*> return s1 <*> mapM renameCallsStat ss2 <*> return s2 <*> return pos
renameCallsStat (While e ss s pos) = While e <$> mapM renameCallsStat ss <*> return s <*> return pos
renameCallsStat (Begin ss pos) = Begin <$> mapM renameCallsStat ss <*> return pos
renameCallsStat s = return s

renameCallsRVal :: RVal -> ScopedSemanticAnalyser RVal
renameCallsRVal (Call i es pos) = do
  eTypes <- mapM checkExprType es
  return $ Call (addTypesToFuncIdent i eTypes) es pos
renameCallsRVal r = return r