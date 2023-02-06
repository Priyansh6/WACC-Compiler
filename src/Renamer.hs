module Renamer where

import AST

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

type ScopeMap = M.Map Int [Ident]

data ScopeAccum = ScopeAccum { scopeMap :: ScopeMap, 
                               scopeStack :: [Int], 
                               scopeCounter :: Int } deriving (Show, Eq)

getCurrentScope :: ScopeAccum -> Int
getCurrentScope = L.head . scopeStack

getScopedVars :: ScopeAccum -> Int -> [Ident]
getScopedVars scopeAccum s = M.findWithDefault [] s (scopeMap scopeAccum)

rename :: Program -> (ScopeMap, Program)
rename prog
  = (scopeMap scopeAccum', prog')
  where 
    (scopeAccum', prog') = renameProg scopeAccum prog
    scopeAccum = ScopeAccum { scopeMap = M.empty, scopeStack = [0],  scopeCounter = 0}

renameProg :: ScopeAccum -> Program -> (ScopeAccum, Program)
renameProg scopeAccum (Program funcs stat)
  = (scopeAccum'', Program funcs' stat')
    where 
      (scopeAccum', funcs') = L.mapAccumL renameFunc scopeAccum funcs
      (scopeAccum'', stat') = renameStat scopeAccum' stat

-- Error handling

renameFunc :: ScopeAccum -> Func -> (ScopeAccum, Func)
renameFunc scopeAccum (Func t ident params stat)
  | L.elem ident (getScopedVars scopeAccum 0) = error("function defined already")
  | otherwise                                 = (scopeAccum'''' { scopeStack = scopeStack scopeAccum } , Func t ident params' stat')
  where
    scopeAccum' = scopeAccum { scopeMap = M.insert 0 (ident : getScopedVars scopeAccum 0) (scopeMap scopeAccum) }
    scopeAccum'' = prepareNewScope scopeAccum scopeAccum'
    (types, idents) = L.unzip params
    (scopeAccum''', idents') = L.mapAccumL renameUndeclaredIdent scopeAccum'' idents
    params' = L.zip types idents'
    (scopeAccum'''', stat') = renameStat scopeAccum''' stat

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

prepareNewScope :: ScopeAccum -> ScopeAccum -> ScopeAccum
prepareNewScope oldScopeAccum previousScopeAccum
  = previousScopeAccum { scopeStack = scopeCounter previousScopeAccum + 1 : scopeStack oldScopeAccum, 
                         scopeCounter = scopeCounter previousScopeAccum + 1 }

renameStat :: ScopeAccum -> Stat -> (ScopeAccum, Stat)
renameStat scopeAccum Skip = (scopeAccum, Skip)
renameStat scopeAccum (DecAssign t ident rVal)
  = (scopeAccum'', DecAssign t ident' rVal')
  where
    (scopeAccum', rVal') = renameRVal scopeAccum rVal
    (scopeAccum'', ident') = renameUndeclaredIdent scopeAccum' ident
renameStat scopeAccum (Assign lVal rVal)
  = (scopeAccum'', Assign lVal' rVal')
  where
    (scopeAccum', rVal') = renameRVal scopeAccum rVal
    (scopeAccum'', lVal') = renameLVal scopeAccum' lVal
renameStat scopeAccum (Read lVal) = mapSnd Read (renameLVal scopeAccum lVal)
renameStat scopeAccum (Free expr) = mapSnd Free (renameExpr scopeAccum expr)
renameStat scopeAccum (Return expr) = mapSnd Return (renameExpr scopeAccum expr)
renameStat scopeAccum (Exit expr) = mapSnd Exit (renameExpr scopeAccum expr)
renameStat scopeAccum (Print expr) = mapSnd Print (renameExpr scopeAccum expr)
renameStat scopeAccum (Println expr) = mapSnd Println (renameExpr scopeAccum expr)
renameStat scopeAccum (If expr stat1 stat2)
  = (scopeAccum''' { scopeStack = scopeStack scopeAccum }, If expr' stat1' stat2')
  where
    (scopeAccum', expr') = renameExpr scopeAccum expr
    (scopeAccum'', stat1') = renameStat (prepareNewScope scopeAccum scopeAccum') stat1
    (scopeAccum''', stat2') = renameStat (prepareNewScope scopeAccum scopeAccum'') stat2
renameStat scopeAccum (While expr stat)
  = (scopeAccum'' { scopeStack = scopeStack scopeAccum }, While expr' stat')
  where
    (scopeAccum', expr') = renameExpr scopeAccum expr
    (scopeAccum'', stat') = renameStat (prepareNewScope scopeAccum scopeAccum') stat
renameStat scopeAccum (Begin stat)
  = (scopeAccum' { scopeStack = scopeStack scopeAccum }, Begin stat')
  where
    (scopeAccum', stat') = renameStat (prepareNewScope scopeAccum scopeAccum) stat
renameStat scopeAccum (Seq stat1 stat2)
  = (scopeAccum'' { scopeStack = scopeStack scopeAccum }, Seq stat1' stat2')
  where
    (scopeAccum', stat1') = renameStat (prepareNewScope scopeAccum scopeAccum) stat1
    (scopeAccum'', stat2') = renameStat (prepareNewScope scopeAccum scopeAccum') stat2

renameLVal :: ScopeAccum -> LVal -> (ScopeAccum, LVal)
renameLVal scopeAccum (LIdent i) = mapSnd LIdent (renameDeclaredIdent scopeAccum i)
renameLVal scopeAccum (LArray arrayElem) = mapSnd LArray (renameArrayElem scopeAccum arrayElem)
renameLVal scopeAccum (LPair pairElem) = mapSnd LPair (renamePairElem scopeAccum pairElem)

renameRVal :: ScopeAccum -> RVal -> (ScopeAccum, RVal)
renameRVal scopeAccum (RExpr expr) = mapSnd RExpr (renameExpr scopeAccum expr)
renameRVal scopeAccum (ArrayLiter exprs) = mapSnd ArrayLiter (L.mapAccumL renameExpr scopeAccum exprs)
renameRVal scopeAccum (NewPair expr1 expr2)
  = (scopeAccum'', NewPair expr1' expr2')
  where
    (scopeAccum', expr1') = renameExpr scopeAccum expr1
    (scopeAccum'', expr2') = renameExpr scopeAccum' expr2
renameRVal scopeAccum (RPair pairElem) = mapSnd RPair (renamePairElem scopeAccum pairElem)
renameRVal scopeAccum (Call i exprs)
  | not (L.elem i (getScopedVars scopeAccum 0)) = error("Function not defined")
  | otherwise                                   = (scopeAccum'', Call i' exprs')
  where
    (scopeAccum', i') = renameDeclaredIdent scopeAccum i
    (scopeAccum'', exprs') = L.mapAccumL renameExpr scopeAccum' exprs

renameExpr :: ScopeAccum -> Expr -> (ScopeAccum, Expr)
renameExpr scopeAccum (IdentExpr i) = mapSnd IdentExpr (renameDeclaredIdent scopeAccum i)
renameExpr scopeAccum (ArrayExpr arrayElem) = mapSnd ArrayExpr (renameArrayElem scopeAccum arrayElem)
renameExpr scopeAccum (Not expr) = mapSnd Not (renameExpr scopeAccum expr)
renameExpr scopeAccum (Neg expr) = mapSnd Neg (renameExpr scopeAccum expr)
renameExpr scopeAccum (Len expr) = mapSnd Len (renameExpr scopeAccum expr)
renameExpr scopeAccum (Ord expr) = mapSnd Ord (renameExpr scopeAccum expr)
renameExpr scopeAccum (Chr expr) = mapSnd Chr (renameExpr scopeAccum expr)
renameExpr scopeAccum (expr1 :*: expr2)
  = (scopeAccum', expr1' :*: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :/: expr2)
  = (scopeAccum', expr1' :/: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :%: expr2)
  = (scopeAccum', expr1' :%: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :+: expr2)
  = (scopeAccum', expr1' :+: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :-: expr2)
  = (scopeAccum', expr1' :-: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :>: expr2)
  = (scopeAccum', expr1' :>: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :>=: expr2)
  = (scopeAccum', expr1' :>=: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :<: expr2)
  = (scopeAccum', expr1' :<: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :<=: expr2)
  = (scopeAccum', expr1' :<=: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :==: expr2)
  = (scopeAccum', expr1' :==: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :!=: expr2)
  = (scopeAccum', expr1' :!=: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :&&: expr2)
  = (scopeAccum', expr1' :&&: expr2')
  where
    (scopeAccum', [expr1', expr2']) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum (expr1 :||: expr2)
  = (scopeAccum', expr1' :||: expr2')
  where
    (scopeAccum', (expr1':expr2':_)) = L.mapAccumL renameExpr scopeAccum [expr1, expr2]
renameExpr scopeAccum expr
  = (scopeAccum, expr)

renameArrayElem :: ScopeAccum -> ArrayElem -> (ScopeAccum, ArrayElem)
renameArrayElem scopeAccum (ArrayElem i exprs)
  = (scopeAccum'', ArrayElem i' exprs')
  where
    (scopeAccum', exprs') = L.mapAccumL renameExpr scopeAccum exprs
    (scopeAccum'', i') = renameDeclaredIdent scopeAccum' i

renamePairElem :: ScopeAccum -> PairElem -> (ScopeAccum, PairElem)
renamePairElem scopeAccum (Fst lVal) = mapSnd Fst (renameLVal scopeAccum lVal)
renamePairElem scopeAccum (Snd lVal) = mapSnd Snd (renameLVal scopeAccum lVal)

addScopeToIdent :: Int -> Ident -> Ident
addScopeToIdent scope (Ident i)
  = Ident ((T.append i . T.append (T.singleton '-') . T.pack . show) scope)

getOriginalIdent :: Ident -> Ident
getOriginalIdent (Ident i) = Ident (T.takeWhile ('-' /=) i)

-- Error handling

renameUndeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameUndeclaredIdent scopeAccum name@(Ident i)
  | L.elem name' (getScopedVars scopeAccum s) = error(msg)
  | otherwise                                 = (scopeAccum', name')
  where
    msg = "Error: Variable " ++ T.unpack i ++ " already defined."
    s = getCurrentScope scopeAccum
    scopeAccum' = scopeAccum { scopeMap = M.insert s (name' : getScopedVars scopeAccum s) (scopeMap scopeAccum)}
    name' = addScopeToIdent s name

-- Error handling

renameDeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameDeclaredIdent scopeAccum ident
  | isNothing ident' = error("fuck you")
  | otherwise        = (scopeAccum, fromJust ident')
  where
    ident' = findInScopeStack (scopeStack scopeAccum) ident
    
    findInScopeStack :: [Int] -> Ident -> Maybe Ident
    findInScopeStack [] _ = Nothing
    findInScopeStack (scope:scopes) name
      | L.elem name' (getScopedVars scopeAccum scope) = Just name'
      | otherwise                                     = findInScopeStack scopes name
      where
        name' = addScopeToIdent scope name