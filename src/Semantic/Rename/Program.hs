module Semantic.Rename.Program where

import qualified Data.List as L
import qualified Data.Map as M

import AST
import Semantic.Errors
import Semantic.Rename.Function (renameFunc)
import Semantic.Rename.Utils
import Semantic.Rename.Scope
import Semantic.Rename.Statement (renameStat)

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