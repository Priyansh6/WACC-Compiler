module Semantic.Rename.RLValExpr where 

import qualified Data.List as L
import Data.Maybe

import AST
import Semantic.Errors
import Semantic.Rename.Helpers
import Semantic.Rename.Scope

renameLVal :: ScopeAccum -> LVal -> (ScopeAccum, LVal)
renameLVal scopeAccum (LIdent i) = chain renameDeclaredIdent i (scopeAccum, LIdent)
renameLVal scopeAccum (LArray arrayElem) = chain renameArrayElem arrayElem (scopeAccum, LArray)
renameLVal scopeAccum (LPair pairElem) = chain renamePairElem pairElem (scopeAccum, LPair)

renameRVal :: ScopeAccum -> RVal -> (ScopeAccum, RVal)
renameRVal scopeAccum (RExpr expr) = chain renameExpr expr (scopeAccum, RExpr)
renameRVal scopeAccum (ArrayLiter exprs pos) = mapSndFunc pos $ chain (L.mapAccumL renameExpr) exprs (scopeAccum, ArrayLiter)
renameRVal scopeAccum (NewPair expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, NewPair)
renameRVal scopeAccum (RPair pairElem) = chain renamePairElem pairElem (scopeAccum, RPair)
renameRVal scopeAccum (Call name exprs pos) =
  mapSndFunc pos $ chain (L.mapAccumL renameExpr) exprs (scopeAccum', Call name)
  where
    scopeAccum' =
      if name `L.elem` getScopedVars scopeAccum 0
        then scopeAccum
        else scopeAccum {errors = FunctionNotDefined name : errors scopeAccum}

renameExpr :: ScopeAccum -> Expr -> (ScopeAccum, Expr)
renameExpr scopeAccum (IdentExpr i pos) = mapSndFunc pos $ chain renameDeclaredIdent i (scopeAccum, IdentExpr)
renameExpr scopeAccum (ArrayExpr arrayElem pos) = mapSndFunc pos $ chain renameArrayElem arrayElem (scopeAccum, ArrayExpr)
renameExpr scopeAccum (Not expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Not)
renameExpr scopeAccum (Neg expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Neg)
renameExpr scopeAccum (Len expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Len)
renameExpr scopeAccum (Ord expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Ord)
renameExpr scopeAccum (Chr expr pos) = mapSndFunc pos $ chain renameExpr expr (scopeAccum, Chr)
renameExpr scopeAccum expr
  | isLiteralExpr expr = (scopeAccum, expr)
  | otherwise = mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, constructor)
  where
    (constructor, expr1, expr2, pos) = unpackBinExpr expr

renameArrayElem :: ScopeAccum -> ArrayElem -> (ScopeAccum, ArrayElem)
renameArrayElem scopeAccum (ArrayElem i exprs pos) =
  mapSndFunc pos $ (chain (L.mapAccumL renameExpr) exprs . chain renameDeclaredIdent i) (scopeAccum, ArrayElem)

renamePairElem :: ScopeAccum -> PairElem -> (ScopeAccum, PairElem)
renamePairElem scopeAccum (Fst lVal pos) = mapSndFunc pos $ chain renameLVal lVal (scopeAccum, Fst)
renamePairElem scopeAccum (Snd lVal pos) = mapSndFunc pos $ chain renameLVal lVal (scopeAccum, Snd)

renameDeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameDeclaredIdent scopeAccum name =
  (scopeAccum', fromMaybe name name')
  where
    name' = findInScopeStack (scopeStack scopeAccum) name
    scopeAccum' = if isNothing name' then scopeAccum {errors = VariableNotDefined name : errors scopeAccum} else scopeAccum
    findInScopeStack :: [Int] -> Ident -> Maybe Ident
    findInScopeStack [] _ = Nothing
    findInScopeStack (scope : scopes) ident
      | ident' `L.elem` getScopedVars scopeAccum scope = Just ident'
      | otherwise = findInScopeStack scopes ident
      where
        ident' = addScopeToIdent scope ident
