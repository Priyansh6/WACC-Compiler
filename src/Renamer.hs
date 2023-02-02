{-# LANGUAGE OverloadedStrings #-}

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
    scopeAccum = ScopeAccum { vars = M.empty, scopeStack = [],  scopeCounter = 0}

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

renameStat :: ScopeAccum -> Stat -> (ScopeAccum, Stat)
renameStat scopeAccum Skip = (scopeAccum, Skip)
renameStat scopeAccum (DecAssign type ident rVal)
  = (scopeAccum'', Decassign type ident' rVal')
  where
    (scopeAccum', rVal') = renameRVal scopeAccum rVal
    (scopeAccum'', ident') = renameUndeclaredIdent scopeAccum' ident
renameStat scopeAccum (Assign lVal rVal)
  = (scopeAccum'', Assign lVal' rVal')
  where
    (scopeAccum', rVal') = renameRVal scopeAccum rVal
    (scopeAccum'', lVal') = renameLVal scopeAccum' lVal
renameStat scopeAccum (Read lVal)
  = (scopeAccum', Read lVal')
  where
    (scopeAccum', lVal') = renameLVal scopeAccum lVal
renameStat scopeAccum (Free expr)
  = (scopeAccum', Free expr')
  where
    (scopeAccum', expr') = renameExpr scopeAccum expr
renameStat scopeAccum (Return expr)
  = (scopeAccum', Return expr')
  where
    (scopeAccum', expr') = renameExpr scopeAccum expr
renameStat scopeAccum (Exit expr)
  = (scopeAccum', Exit expr')
  where
    (scopeAccum', expr') = renameExpr scopeAccum expr
renameStat scopeAccum (Print expr)
  = (scopeAccum', Print expr')
  where
    (scopeAccum', expr') = renameExpr scopeAccum expr
renameStat scopeAccum (Println expr)
  = (scopeAccum', Println expr')
  where
    (scopeAccum', expr') = renameExpr scopeAccum expr

renameUndeclaredIdent :: ScopeAccum -> Ident -> (ScopeAccum, Ident)
renameUndeclaredIdent scopeAccum name@(Ident i)
  | member name' (vars scopeAccum) = error(msg)
  | otherwise                      = (scopeAccum', name')
  where
    msg = "Error: Variable " ++ i ++ " already defined."
    scopeAccum' = scopeAccum { vars = M.insert name' name (vars scopeAccum) }
    name' = Ident ((append i . show . scopeCounter) scopeAccum)