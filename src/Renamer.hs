module Renamer (module Renamer) where

import AST

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

type ScopeMap = M.Map Int [Ident]

data ScopeAccum = ScopeAccum { scopeMap :: ScopeMap, 
                               scopeStack :: [Int], 
                               scopeCounter :: Int,
                               errors :: [String]} deriving (Show, Eq)

initialScopeAccum :: ScopeAccum
initialScopeAccum = ScopeAccum { scopeMap = M.empty, 
                                 scopeStack = [0], 
                                 scopeCounter = 0, 
                                 errors = [] }

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

getCurrentScope :: ScopeAccum -> Int
getCurrentScope = L.head . scopeStack

getScopedVars :: ScopeAccum -> Int -> [Ident]
getScopedVars scopeAccum s = M.findWithDefault [] s (scopeMap scopeAccum)

prepareNewScope :: ScopeAccum -> ScopeAccum -> ScopeAccum
prepareNewScope oldScopeAccum scopeAccum
  = scopeAccum { scopeStack = scopeCounter scopeAccum + 1 : scopeStack oldScopeAccum, 
                 scopeCounter = scopeCounter scopeAccum + 1 }

resetScope :: ScopeAccum -> ScopeAccum -> ScopeAccum
resetScope oldScopeAccum scopeAccum
  = scopeAccum { scopeStack = scopeStack oldScopeAccum }

chain :: (acc -> x -> (acc, x)) -> x -> (acc, x -> y) -> (acc, y)
chain accFunc x (acc, f)
  = mapSnd f (accFunc acc x)

chainNewScope :: (ScopeAccum -> x -> (ScopeAccum, x)) -> x -> ScopeAccum -> (ScopeAccum, x -> y) -> (ScopeAccum, y)
chainNewScope accFunc x oldScopeAccum (acc, f)
  = mapSnd f (accFunc (prepareNewScope oldScopeAccum acc) x)

chainResetScope :: ScopeAccum -> (ScopeAccum, x) -> (ScopeAccum, x)
chainResetScope oldScopeAccum = mapFst (resetScope oldScopeAccum)

rename :: Program -> ((ScopeMap, [String]), Program)
rename prog
  = mapFst (\sa -> (scopeMap sa, errors sa)) (renameProg initialScopeAccum prog)

renameProg :: ScopeAccum -> Program -> (ScopeAccum, Program)
renameProg scopeAccum (Program funcs stats)
  = (chain (L.mapAccumL renameStat) stats . chain (L.mapAccumL renameFunc) funcs) (scopeAccum, Program)

renameFunc :: ScopeAccum -> Func -> (ScopeAccum, Func)
renameFunc scopeAccum (Func t name@(Ident i) params stats)
  | otherwise = (chainResetScope scopeAccum . chain (L.mapAccumL renameStat) stats . 
                 chainNewScope (L.mapAccumL renameParam) params scopeAccum) (scopeAccum'', Func t name)
  where
    scopeAccum' = scopeAccum { scopeMap = M.insert 0 (name : getScopedVars scopeAccum 0) (scopeMap scopeAccum) }
    scopeAccum'' = if L.elem name (getScopedVars scopeAccum 0)
                      then scopeAccum { errors = errorMsg : errors scopeAccum } 
                      else scopeAccum'
    errorMsg = "Error: Function " ++ T.unpack i ++ " already defined."

renameParam :: ScopeAccum -> (WType, Ident) -> (ScopeAccum, (WType, Ident))
renameParam scopeAccum (t, name)
  = mapSnd (\n -> (t, n)) (renameUndeclaredIdent scopeAccum name)

renameStat :: ScopeAccum -> Stat -> (ScopeAccum, Stat)
renameStat scopeAccum Skip = (scopeAccum, Skip)
renameStat scopeAccum (DecAssign t ident rVal)
  = (chain renameUndeclaredIdent ident . chain renameRVal rVal) (scopeAccum, flip (DecAssign t))
renameStat scopeAccum (Assign lVal rVal)
  = (chain renameLVal lVal . chain renameRVal rVal) (scopeAccum, flip Assign)
renameStat scopeAccum (Read lVal) = chain renameLVal lVal (scopeAccum, Read)
renameStat scopeAccum (Free expr) = chain renameExpr expr (scopeAccum, Free)
renameStat scopeAccum (Return expr) = chain renameExpr expr (scopeAccum, Return)
renameStat scopeAccum (Exit expr) = chain renameExpr expr (scopeAccum, Exit)
renameStat scopeAccum (Print expr) = chain renameExpr expr (scopeAccum, Print)
renameStat scopeAccum (Println expr) = chain renameExpr expr (scopeAccum, Println)
renameStat scopeAccum (If expr stats1 stats2)
  = (chainResetScope scopeAccum . chainNewScope (L.mapAccumL renameStat) stats2 scopeAccum .
     chainNewScope (L.mapAccumL renameStat) stats1 scopeAccum . chain renameExpr expr) (scopeAccum, If)
renameStat scopeAccum (While expr stats)
  = (chainResetScope scopeAccum . chainNewScope (L.mapAccumL renameStat) stats scopeAccum .
     chain renameExpr expr) (scopeAccum, While)
renameStat scopeAccum (Begin stats)
  = (chainResetScope scopeAccum . chainNewScope (L.mapAccumL renameStat) stats scopeAccum) (scopeAccum, Begin)

renameLVal :: ScopeAccum -> LVal -> (ScopeAccum, LVal)
renameLVal scopeAccum (LIdent i) = chain renameDeclaredIdent i (scopeAccum, LIdent)
renameLVal scopeAccum (LArray arrayElem) = chain renameArrayElem arrayElem (scopeAccum, LArray)
renameLVal scopeAccum (LPair pairElem) = chain renamePairElem pairElem (scopeAccum, LPair)

renameRVal :: ScopeAccum -> RVal -> (ScopeAccum, RVal)
renameRVal scopeAccum (RExpr expr) = chain renameExpr expr (scopeAccum, RExpr)
renameRVal scopeAccum (ArrayLiter exprs) = chain (L.mapAccumL renameExpr) exprs (scopeAccum, ArrayLiter)
renameRVal scopeAccum (NewPair expr1 expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, NewPair)
renameRVal scopeAccum (RPair pairElem) = chain renamePairElem pairElem (scopeAccum, RPair)
renameRVal scopeAccum (Call name@(Ident i) exprs)
  = chain (L.mapAccumL renameExpr) exprs (scopeAccum', Call name)
  where
    scopeAccum' = if not (L.elem name (getScopedVars scopeAccum 0)) 
                      then scopeAccum { errors = errorMsg : errors scopeAccum } 
                      else scopeAccum
    errorMsg = "Error: Function " ++ T.unpack i ++ " not defined."

renameExpr :: ScopeAccum -> Expr -> (ScopeAccum, Expr)
renameExpr scopeAccum (IdentExpr i) = chain renameDeclaredIdent i (scopeAccum, IdentExpr)
renameExpr scopeAccum (ArrayExpr arrayElem) = chain renameArrayElem arrayElem (scopeAccum, ArrayExpr)
renameExpr scopeAccum (Not expr) = chain renameExpr expr (scopeAccum, Not)
renameExpr scopeAccum (Neg expr) = chain renameExpr expr (scopeAccum, Neg)
renameExpr scopeAccum (Len expr) = chain renameExpr expr (scopeAccum, Len)
renameExpr scopeAccum (Ord expr) = chain renameExpr expr (scopeAccum, Ord)
renameExpr scopeAccum (Chr expr) = chain renameExpr expr (scopeAccum, Chr)
renameExpr scopeAccum (expr1 :*: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:*:))
renameExpr scopeAccum (expr1 :/: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:/:))
renameExpr scopeAccum (expr1 :%: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:%:))
renameExpr scopeAccum (expr1 :+: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:+:))
renameExpr scopeAccum (expr1 :-: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:-:))
renameExpr scopeAccum (expr1 :>: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:>:))
renameExpr scopeAccum (expr1 :>=: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:>=:))
renameExpr scopeAccum (expr1 :<: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:<:))
renameExpr scopeAccum (expr1 :<=: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:<=:))
renameExpr scopeAccum (expr1 :==: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:==:))
renameExpr scopeAccum (expr1 :!=: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:!=:))
renameExpr scopeAccum (expr1 :&&: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:&&:))
renameExpr scopeAccum (expr1 :||: expr2)
  = (chain renameExpr expr2 . chain renameExpr expr1) (scopeAccum, (:||:))
renameExpr scopeAccum expr
  = (scopeAccum, expr)

renameArrayElem :: ScopeAccum -> ArrayElem -> (ScopeAccum, ArrayElem)
renameArrayElem scopeAccum (ArrayElem i exprs)
  = (chain (L.mapAccumL renameExpr) exprs . chain renameDeclaredIdent i) (scopeAccum, ArrayElem)

renamePairElem :: ScopeAccum -> PairElem -> (ScopeAccum, PairElem)
renamePairElem scopeAccum (Fst lVal) = chain renameLVal lVal (scopeAccum, Fst)
renamePairElem scopeAccum (Snd lVal) = chain renameLVal lVal (scopeAccum, Snd)

addScopeToIdent :: Int -> Ident -> Ident
addScopeToIdent scope (Ident i)
  = Ident ((T.append i . T.append (T.singleton '-') . T.pack . show) scope)

getOriginalIdent :: Ident -> Ident
getOriginalIdent (Ident i) = Ident (T.takeWhile ('-' /=) i)

renameUndeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameUndeclaredIdent scopeAccum name@(Ident i)
  = (scopeAccum'', name')
  where
    s = getCurrentScope scopeAccum
    scopeAccum' = scopeAccum { scopeMap = M.insert s (name' : getScopedVars scopeAccum s) (scopeMap scopeAccum)}
    scopeAccum'' = if L.elem name' (getScopedVars scopeAccum s) 
                      then scopeAccum { errors = errorMsg : errors scopeAccum } 
                      else scopeAccum'
    name' = addScopeToIdent s name
    errorMsg = "Error: Variable " ++ T.unpack i ++ " already defined."

renameDeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameDeclaredIdent scopeAccum name@(Ident i)
  = (scopeAccum', fromMaybe name name')
  where
    name' = findInScopeStack (scopeStack scopeAccum) name
    scopeAccum' = if isNothing name' then scopeAccum { errors = errorMsg : errors scopeAccum } else scopeAccum
    errorMsg = "Error: Variable " ++ T.unpack i ++ " not defined."
    
    findInScopeStack :: [Int] -> Ident -> Maybe Ident
    findInScopeStack [] _ = Nothing
    findInScopeStack (scope:scopes) ident
      | L.elem ident' (getScopedVars scopeAccum scope) = Just ident'
      | otherwise                                     = findInScopeStack scopes ident
      where
        ident' = addScopeToIdent scope ident