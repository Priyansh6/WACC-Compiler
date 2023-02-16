module Renamer (module Renamer) where

import AST
import RenameHelpers
import SemanticErrors
import Scope
import RenameFunc (renameFunc)
import RenameStat (renameStat)

import qualified Data.List as L
import qualified Data.Map as M

rename :: Program -> ((ScopeMap, [SemanticError]), Program)
rename prog =
  mapFst (\sa -> (scopeMap sa, reverse $ errors sa)) (renameProg initialScopeAccum prog)

renameProg :: ScopeAccum -> Program -> (ScopeAccum, Program)
renameProg scopeAccum (Program funcs stats) =
  (chain (L.mapAccumL renameStat) stats . chain (L.mapAccumL renameFunc) funcs) (scopeAccum', Program)
  where
    scopeAccum' = foldl addFuncName scopeAccum funcs

addFuncName :: ScopeAccum -> Func -> ScopeAccum
addFuncName scopeAccum (Func _ name@(Ident _ _) _ _ _ _)
  | name `L.elem` getScopedVars scopeAccum 0 = scopeAccum {errors = FunctionAlreadyDefined name : errors scopeAccum}
  | otherwise                                = scopeAccum'
  where
    scopeAccum' = scopeAccum {scopeMap = M.insert 0 (name : getScopedVars scopeAccum 0) (scopeMap scopeAccum)}