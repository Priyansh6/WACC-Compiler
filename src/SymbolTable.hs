module SymbolTable (module SymbolTable) where

import AST
import Data.Text
import qualified Data.List as L
import qualified Data.Map as M

type SymbolTable = M.Map Ident IdentType

data IdentType =  FuncType WType [Ident] 
                | VarType WType
                | ArrType WType Int
                | PairType WType WType
                deriving(Show, Eq)

checkProg :: Program  -> SymbolTable
checkProg (Program funcs stats) = st'
  where 
    st = checkFuncs M.empty funcs
    st' = checkStats st stats

checkFuncs :: SymbolTable -> [Func] -> SymbolTable
checkFuncs = L.foldl checkFunc

checkFunc :: SymbolTable -> Func -> SymbolTable
checkFunc st (Func wtype ident params stats) = st'''
  where
    ( _ , paramIds) = unzip params
    st' = M.insert ident (FuncType wtype paramIds) st
    st'' = insertParams params st'
    st''' = checkStats  st'' stats

checkStats :: SymbolTable -> Stats -> SymbolTable
checkStats = L.foldl checkStat

checkStat :: SymbolTable -> Stat -> SymbolTable
checkStat st (DecAssign wtype ident _ ) = 
  insertAssign st wtype ident
checkStat st _ = st

insertAssign :: SymbolTable -> WType -> Ident -> SymbolTable
insertAssign st (WArr wtype int) ident = M.insert ident (ArrType wtype int) st
insertAssign st (WPair wtype1 wtype2) ident = M.insert ident (PairType wtype1 wtype2) st
insertAssign st wtype ident = M.insert ident (VarType wtype) st 


insertParams :: [(WType, Ident)] -> SymbolTable -> SymbolTable
insertParams [] st = st
insertParams ((wType, ident):params) st 
  = insertParams params (M.insert ident (VarType wType) st) 


