module Semantic.Rename.Scope where 

import qualified Data.List as L
import qualified Data.Map as M

import AST
import Semantic.Errors
import Semantic.Rename.Helpers

type ScopeMap = M.Map Int [Ident]

data ScopeAccum = ScopeAccum
  { scopeMap :: ScopeMap,
    scopeStack :: [Int],
    scopeCounter :: Int,
    errors :: [SemanticError]
  }
  deriving (Show, Eq)

initialScopeAccum :: ScopeAccum
initialScopeAccum =
  ScopeAccum
    { scopeMap = M.empty,
      scopeStack = [0],
      scopeCounter = 0,
      errors = []
    }
    
prepareNewScope :: ScopeAccum -> ScopeAccum
prepareNewScope scopeAccum =
  scopeAccum
    { scopeStack = scopeCounter scopeAccum + 1 : scopeStack scopeAccum,
      scopeCounter = scopeCounter scopeAccum + 1
    }

getCurrentScope :: ScopeAccum -> Int
getCurrentScope = L.head . scopeStack

getScopedVars :: ScopeAccum -> Int -> [Ident]
getScopedVars scopeAccum s = M.findWithDefault [] s (scopeMap scopeAccum)

resetScope :: ScopeAccum -> ScopeAccum -> ScopeAccum
resetScope oldScopeAccum scopeAccum =
  scopeAccum {scopeStack = scopeStack oldScopeAccum}

chainAddScope :: (ScopeAccum, Scope -> x) -> (ScopeAccum, x)
chainAddScope (acc, f) =
  (acc, f $ Just $ getCurrentScope acc)

chainNewScope :: (ScopeAccum -> x -> (ScopeAccum, x)) -> x -> (ScopeAccum, x -> y) -> (ScopeAccum, y)
chainNewScope accFunc x (acc, f) =
  mapSnd f (accFunc (prepareNewScope acc) x)

chainResetScope :: ScopeAccum -> (ScopeAccum, x) -> (ScopeAccum, x)
chainResetScope oldScopeAccum = mapFst (resetScope oldScopeAccum)
