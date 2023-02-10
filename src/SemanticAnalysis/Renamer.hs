module SemanticAnalysis.Renamer (module SemanticAnalysis.Renamer) where

import AST
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import SemanticAnalysis.SemanticErrors

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

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSndFunc :: b -> (a, b -> c) -> (a, c)
mapSndFunc y (x, f) = (x, f y)

getCurrentScope :: ScopeAccum -> Int
getCurrentScope = L.head . scopeStack

getScopedVars :: ScopeAccum -> Int -> [Ident]
getScopedVars scopeAccum s = M.findWithDefault [] s (scopeMap scopeAccum)

prepareNewScope :: ScopeAccum -> ScopeAccum
prepareNewScope scopeAccum =
  scopeAccum
    { scopeStack = scopeCounter scopeAccum + 1 : scopeStack scopeAccum,
      scopeCounter = scopeCounter scopeAccum + 1
    }

resetScope :: ScopeAccum -> ScopeAccum -> ScopeAccum
resetScope oldScopeAccum scopeAccum =
  scopeAccum {scopeStack = scopeStack oldScopeAccum}

chain :: (acc -> x -> (acc, x)) -> x -> (acc, x -> y) -> (acc, y)
chain accFunc x (acc, f) =
  mapSnd f (accFunc acc x)

chainNewScope :: (ScopeAccum -> x -> (ScopeAccum, x)) -> x -> (ScopeAccum, x -> y) -> (ScopeAccum, y)
chainNewScope accFunc x (acc, f) =
  mapSnd f (accFunc (prepareNewScope acc) x)

chainResetScope :: ScopeAccum -> (ScopeAccum, x) -> (ScopeAccum, x)
chainResetScope oldScopeAccum = mapFst (resetScope oldScopeAccum)

rename :: Program -> ((ScopeMap, [SemanticError]), Program)
rename prog =
  mapFst (\sa -> (scopeMap sa, reverse $ errors sa)) (renameProg initialScopeAccum prog)

addFuncName :: ScopeAccum -> Func -> ScopeAccum
addFuncName scopeAccum (Func _ name@(Ident _ _) _ _ _)
  | name `L.elem` getScopedVars scopeAccum 0 = scopeAccum {errors = FunctionAlreadyDefined name : errors scopeAccum}
  | otherwise                                = scopeAccum'
  where
    scopeAccum' = scopeAccum {scopeMap = M.insert 0 (name : getScopedVars scopeAccum 0) (scopeMap scopeAccum)}

renameProg :: ScopeAccum -> Program -> (ScopeAccum, Program)
renameProg scopeAccum (Program funcs stats) =
  (chain (L.mapAccumL renameStat) stats . chain (L.mapAccumL renameFunc) funcs) (scopeAccum', Program)
  where
    scopeAccum' = foldl addFuncName scopeAccum funcs

renameFunc :: ScopeAccum -> Func -> (ScopeAccum, Func)
renameFunc scopeAccum (Func t name params stats pos) =
  mapSndFunc pos $
    ( chainResetScope scopeAccum
        . chainNewScope (L.mapAccumL renameStat) stats
        . chainNewScope (L.mapAccumL renameParam) params
    )
      (scopeAccum, Func t name)
      
renameParam :: ScopeAccum -> (WType, Ident) -> (ScopeAccum, (WType, Ident))
renameParam scopeAccum (t, name) =
  mapSnd (\n -> (t, n)) (renameUndeclaredIdent scopeAccum name)

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
renameStat scopeAccum (If expr stats1 stats2 pos) =
  mapSndFunc pos $
    ( chainResetScope scopeAccum
        . chainNewScope (L.mapAccumL renameStat) stats2
        . chainResetScope scopeAccum
        . chainNewScope (L.mapAccumL renameStat) stats1
        . chain renameExpr expr
    )
      (scopeAccum, If)
renameStat scopeAccum (While expr stats pos) =
  mapSndFunc pos $
    ( chainResetScope scopeAccum
        . chainNewScope (L.mapAccumL renameStat) stats
        . chain renameExpr expr
    )
      (scopeAccum, While)
renameStat scopeAccum (Begin stats) =
  (chainResetScope scopeAccum . chainNewScope (L.mapAccumL renameStat) stats) (scopeAccum, Begin)

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
renameExpr scopeAccum ((:*:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:*:))
renameExpr scopeAccum ((:/:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:/:))
renameExpr scopeAccum ((:%:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:%:))
renameExpr scopeAccum ((:+:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:+:))
renameExpr scopeAccum ((:-:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:-:))
renameExpr scopeAccum ((:>:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:>:))
renameExpr scopeAccum ((:>=:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:>=:))
renameExpr scopeAccum ((:<:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:<:))
renameExpr scopeAccum ((:<=:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:<=:))
renameExpr scopeAccum ((:==:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:==:))
renameExpr scopeAccum ((:!=:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:!=:))
renameExpr scopeAccum ((:&&:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:&&:))
renameExpr scopeAccum ((:||:) expr1 expr2 pos) =
  mapSndFunc pos $ (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:||:))
renameExpr scopeAccum expr =
  (scopeAccum, expr)

renameArrayElem :: ScopeAccum -> ArrayElem -> (ScopeAccum, ArrayElem)
renameArrayElem scopeAccum (ArrayElem i exprs pos) =
  mapSndFunc pos $ (chain (L.mapAccumL renameExpr) exprs . chain renameDeclaredIdent i) (scopeAccum, ArrayElem)

renamePairElem :: ScopeAccum -> PairElem -> (ScopeAccum, PairElem)
renamePairElem scopeAccum (Fst lVal pos) = mapSndFunc pos $ chain renameLVal lVal (scopeAccum, Fst)
renamePairElem scopeAccum (Snd lVal pos) = mapSndFunc pos $ chain renameLVal lVal (scopeAccum, Snd)

addScopeToIdent :: Int -> Ident -> Ident
addScopeToIdent scope (Ident i pos) =
  Ident ((T.append i . T.append (T.singleton '-') . T.pack . show) scope) pos

getOriginalIdent :: Ident -> Ident
getOriginalIdent (Ident i pos) = Ident (T.takeWhile ('-' /=) i) pos

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