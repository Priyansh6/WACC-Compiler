module Semantic.Rename.Statement where

import qualified Data.List as L
import qualified Data.Map as M

import AST
import Semantic.Errors
import Semantic.Rename.Helpers
import Semantic.Rename.RLValExpr
import Semantic.Rename.Scope

renameStat :: ScopeAccum -> Stat -> (ScopeAccum, Stat)
renameStat scopeAccum Skip = (scopeAccum, Skip)
renameStat scopeAccum (DecAssign t ident rVal pos) =
  mapSndFunc pos $ (chain renameUndeclaredIdent ident . chain renameRVal rVal) (scopeAccum, flip (DecAssign t))
renameStat scopeAccum (Assign lVal rVal pos) =
  mapSndFunc pos $ (chain renameLVal lVal . chain renameRVal rVal) (scopeAccum, flip Assign)
renameStat scopeAccum (Read lVal pos) = mapSndFunc pos $ chain renameLVal lVal (scopeAccum, Read)
renameStat scopeAccum (Free expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Free)
renameStat scopeAccum (Return expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Return)
renameStat scopeAccum (Exit expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Exit)
renameStat scopeAccum (Print expr) = chain renameExpr expr (scopeAccum, Print)
renameStat scopeAccum (Println expr) = chain renameExpr expr (scopeAccum, Println)
renameStat scopeAccum (If expr stats1 _ stats2 _ pos) =
  mapSndFunc pos $
    ( chainResetScope scopeAccum
        . chainAddScope
        . chainNewScope (L.mapAccumL renameStat) stats2
        . chainResetScope scopeAccum
        . chainAddScope
        . chainNewScope (L.mapAccumL renameStat) stats1
        . chain renameExpr expr
    )
      (scopeAccum, If)
renameStat scopeAccum (While expr stats _ pos) =
  mapSndFunc pos $
    ( chainResetScope scopeAccum
        . chainAddScope
        . chainNewScope (L.mapAccumL renameStat) stats
        . chain renameExpr expr
    )
      (scopeAccum, While)
renameStat scopeAccum (Begin stats _) =
  (chainResetScope scopeAccum . chainAddScope . chainNewScope (L.mapAccumL renameStat) stats) (scopeAccum, Begin)

renameUndeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameUndeclaredIdent scopeAccum name =
  (scopeAccum'', name'')
  where
    s = getCurrentScope scopeAccum
    scopeAccum' = scopeAccum {scopeMap = M.insert s (name' : getScopedVars scopeAccum s) (scopeMap scopeAccum)}
    (scopeAccum'', name'') =
      if name' `L.elem` getScopedVars scopeAccum s
        then (scopeAccum {errors = VariableAlreadyDefined name : errors scopeAccum}, name)
        else (scopeAccum', name')
    name' = addScopeToIdent s name