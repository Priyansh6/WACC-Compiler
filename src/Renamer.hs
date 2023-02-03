module Renamer where

import AST

import Data.Map as M
import Data.List as L
import Data.Text as T

type VarMap = M.Map Ident Ident

data ScopeAccum = ScopeAccum { vars :: VarMap, 
                               scopeStack :: [Int], 
                               scopeCounter :: Int } deriving (Show, Eq)

rename :: Program -> (VarMap, Program)
rename prog
  = (vars scopeAccum', prog')
  where 
    (scopeAccum', prog') = renameProg scopeAccum prog
    scopeAccum = ScopeAccum { vars = M.empty, scopeStack = [0],  scopeCounter = 0}

renameProg :: ScopeAccum -> Program -> (ScopeAccum, Program)
renameProg scopeAccum (Program funcs stat)
  = undefined
    where 
      (scopeAccum', renamedFuncs) = L.mapAccumL renameFunc scopeAccum funcs


renameFunc :: ScopeAccum -> Func -> (ScopeAccum, Func)
renameFunc scopeAccum (Func t ident params stat)
  = (scopeAccum'', Func t ident renamedParams renamedStat)
  where
    (types, idents) = L.unzip params
    (scopeAccum', renamedIdents) = L.mapAccumL renameUndeclaredIdent scopeAccum idents
    renamedParams = L.zip types renamedIdents
    (scopeAccum'', renamedStat) = renameStat scopeAccum' stat

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
renameStat scopeAccum (Read lVal)
  = mapSnd Read (renameLVal scopeAccum lVal)
renameStat scopeAccum (Free expr)
  = mapSnd Free (renameExpr scopeAccum expr)
renameStat scopeAccum (Return expr)
  = mapSnd Return (renameExpr scopeAccum expr)
renameStat scopeAccum (Exit expr)
  = mapSnd Exit (renameExpr scopeAccum expr)
renameStat scopeAccum (Print expr)
  = mapSnd Print (renameExpr scopeAccum expr)
renameStat scopeAccum (Println expr)
  = mapSnd Println (renameExpr scopeAccum expr)
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
renameLVal scopeAccum lVal = undefined

renameRVal :: ScopeAccum -> RVal -> (ScopeAccum, RVal)
renameRVal scopeAccum rVal = undefined

renameExpr :: ScopeAccum -> Expr -> (ScopeAccum, Expr)
renameExpr scopeAccum Expr = undefined

renameUndeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameUndeclaredIdent scopeAccum name@(Ident i)
  | member name' (vars scopeAccum) = error(msg)
  | otherwise                      = (scopeAccum', name')
  where
    msg = "Error: Variable " ++ T.unpack i ++ " already defined."
    scopeAccum' = scopeAccum { vars = M.insert name' name (vars scopeAccum) }
    name' = Ident ((append i . T.pack . show . scopeCounter) scopeAccum)