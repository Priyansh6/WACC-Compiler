module Semantic.Rename.Function (module Semantic.Rename.Function) where 

import qualified Data.List as L

import AST
import Semantic.Rename.Helpers
import Semantic.Rename.Scope
import Semantic.Rename.Statement

renameFunc :: ScopeAccum -> Func -> (ScopeAccum, Func)
renameFunc scopeAccum (Func t name params stats _ pos) =
  mapSndFunc pos $
    ( chainResetScope scopeAccum
        . chainAddScope
        . chainNewScope (L.mapAccumL renameStat) stats
        . chainNewScope (L.mapAccumL renameParam) params
    )
      (scopeAccum, Func t name)
      
renameParam :: ScopeAccum -> (WType, Ident) -> (ScopeAccum, (WType, Ident))
renameParam scopeAccum (t, name) =
  mapSnd (\n -> (t, n)) (renameUndeclaredIdent scopeAccum name)