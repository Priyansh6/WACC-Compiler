module Semantic.Rename.Statement (renameStat) where

import AST
import Semantic.Rename.Utils
import Semantic.Rename.RLValExpr

renameStat :: Stat -> Renamer Stat
renameStat Skip = return Skip
renameStat (DecAssign t i rVal pos) = flip (DecAssign t) <$> renameRVal rVal <*> renameUndeclaredIdent i <*> return pos
renameStat (Assign lVal rVal pos) = flip Assign <$> renameRVal rVal <*> renameLVal lVal <*> return pos
renameStat (Read lVal pos) = Read <$> renameLVal lVal <*> return pos
renameStat (Free expr pos) = Free <$> renameExpr expr <*> return pos
renameStat (Return expr pos) = Return <$> renameExpr expr <*> return pos
renameStat (Exit expr pos) = Exit <$> renameExpr expr <*> return pos
renameStat (Print expr) = Print <$> renameExpr expr
renameStat (Println expr) = Println <$> renameExpr expr
renameStat (If expr stats1 _ stats2 _ pos) = do
  expr' <- renameExpr expr
  s1 <- nextFreeScope
  stats1' <- prepareNewScope $ mapM renameStat stats1
  s2 <- nextFreeScope
  stats2' <- prepareNewScope $ mapM renameStat stats2
  return $ If expr' stats1' (Just s1) stats2' (Just s2) pos
renameStat (While expr stats _ pos) = do
  expr' <- renameExpr expr
  s <- nextFreeScope
  stats' <- prepareNewScope $ mapM renameStat stats
  return $ While expr' stats' (Just s) pos
renameStat (Begin stats _) = flip Begin <$> (Just <$> nextFreeScope) <*> prepareNewScope (mapM renameStat stats)