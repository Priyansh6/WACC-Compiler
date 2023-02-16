module RenameFunc (module RenameFunc) where 

import AST
import RenameHelpers
import Scope
import RenameStat

import qualified Data.List as L

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