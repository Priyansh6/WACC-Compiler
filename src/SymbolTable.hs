module SymbolTable (checkProg) where

import AST
import Data.Map ((!))
import qualified Data.List as L
import qualified Data.Map as M

type SymbolTable = M.Map Ident IdentType

data Error = Error Ident (Int, Int) 

data IdentType =  FuncType WType [Ident] 
                | VarType WType
                | ArrType WType Int
                | PairType WType WType
                deriving(Show, Eq)

fromIdentType :: IdentType -> WType
fromIdentType (FuncType wtype _) = wtype
fromIdentType (VarType wtype) = wtype
fromIdentType (ArrType wtype _) = wtype
fromIdentType (PairType wtype wtype') = WPair wtype wtype'

checkProg :: Program  -> SymbolTable
checkProg (Program funcs stats) = st'
  where 
    st = checkFuncs M.empty funcs
    st' = checkStats st stats

checkFuncs :: SymbolTable -> [Func] -> SymbolTable
checkFuncs = L.foldl checkFunc 

checkFunc :: SymbolTable -> Func -> SymbolTable
checkFunc st (Func wtype ident params stats) 
  = case last stats of
      Exit _ -> st'''
      Return expr | wtype == checkExprType st''' expr -> st'''
      _ -> error "function type does not match return type"
  where
    ( _ , paramIds) = unzip params
    st' = M.insert ident (FuncType wtype paramIds) st
    st'' = insertParams params st'
    st''' = checkStats st'' stats

checkStats :: SymbolTable -> Stats -> SymbolTable
checkStats = L.foldl checkStat

checkStat :: SymbolTable -> Stat -> SymbolTable
checkStat st (DecAssign wtype ident rval)
  | wtype == checkRVal st rval = st'
  | otherwise = error ("rval type failed in DecAssign " ++ show rval ++ show wtype)
  where
    st' = insertAssign st wtype ident
checkStat st (Assign lval rval)
  | checkLVal st lval == checkRVal st rval = st
  | otherwise = error ("rval type didnt equal lval type in assign " ++ show rval ++ show lval)
checkStat st (Free expr)
  = case checkExprType st expr of
      (WPair _ _) -> st
      (WArr _ _) -> st
      _ -> error ("Free " ++ show expr)
checkStat st (Exit expr)
  | checkExprType st expr == WInt = st
  | otherwise = error ("exit " ++ show expr)
checkStat st (If expr stats1 stats2)
  | checkExprType st expr == WBool = st''
  | otherwise = error ("If " ++ show expr ++ " " ++ show stats1 ++ " " ++ show stats2)
  where 
    st' = checkStats st stats1
    st''= checkStats st' stats2
checkStat st (While expr stats)
  | checkExprType st expr == WBool = checkStats st stats
  | otherwise = error ("while " ++ show expr)
checkStat st (Begin stats) = st'
  where
    st' = checkStats st stats
checkStat st _ = st

checkRVal :: SymbolTable -> RVal -> WType
checkRVal st (RExpr expr) = checkExprType st expr 
checkRVal st (ArrayLiter exprs)
  | allTypesSame exprTypes = head exprTypes
  | otherwise = error ("arraylit " ++ show exprs)
  where 
    exprTypes = map (checkExprType st) exprs
    allTypesSame xs = all (== head xs) (tail xs)
checkRVal st (NewPair e1 e2) = WPair wtype1' wtype2'
  where
    wtype1 = checkExprType st e1
    wtype1' = case wtype1 of 
      WPair _ _ -> WUnit
      _ -> wtype1
    wtype2 = checkExprType st e2
    wtype2' = case wtype2 of 
      WPair _ _ -> WUnit
      _ -> wtype2
checkRVal st (RPair pairElem) = checkPairElemType st pairElem
checkRVal st (Call ident exprs)
  | typesMatchParams = wtype
  | otherwise = error ("function call params " ++ show (Call ident exprs))
  where 
    types = map (checkExprType st) exprs
    paramTypes = map (fromIdentType . (st !)) ids
    typesMatchParams = types == paramTypes
    (wtype, ids) = case st ! ident of 
              FuncType wt idents -> (wt, idents)
              _ -> error ("function call not a function " ++ show (Call ident exprs))

checkLVal :: SymbolTable -> LVal -> WType
checkLVal st (LIdent i) = fromIdentType $ st ! i
checkLVal st (LArray (ArrayElem i es)) 
  | all ((== WInt) . checkExprType st) es = fromIdentType $ st ! i
  | otherwise = error ("lval checkpairelemtype " ++ show (LArray (ArrayElem i es)))
checkLVal st (LPair p) = checkPairElemType st p

checkPairElemType :: SymbolTable -> PairElem -> WType
checkPairElemType st (Fst (LIdent i))
  = case fromIdentType $ st ! i of
    WPair t _ -> t
    _ -> error "taking fst of a non pair"
checkPairElemType st (Fst lval@(LPair _)) = checkLVal st lval
checkPairElemType st (Snd (LIdent i))
  = case fromIdentType $ st ! i of
    WPair _ t -> t
    _ -> error "taking snd of a non pair"
checkPairElemType st (Snd lval@(LPair _)) = checkLVal st lval
checkPairElemType _ _ = error "don't call fst/snd on an array you fucking idiot"

checkExprType :: SymbolTable -> Expr -> WType
checkExprType _ (IntLiter _ ) = WInt 
checkExprType _ (BoolLiter _ ) = WBool 
checkExprType _ (CharLiter _) = WChar
checkExprType _ (StrLiter _ ) = WStr
checkExprType _ PairLiter = WPair WUnit WUnit
checkExprType st (IdentExpr i) = fromIdentType $ st ! i
checkExprType st (ArrayExpr (ArrayElem i _)) = fromIdentType $ st ! i
checkExprType st (Not e) 
  = case checkExprType st e of
    WBool -> WBool
    _ -> error ("Not " ++ show e)
checkExprType st (Neg e) 
  = case checkExprType st e of
    WInt -> WInt
    _ -> error ("Neg " ++ show e)

checkExprType st (Len e) 
  = case checkExprType st e of
    WArr _ _ -> WInt
    _ -> error ("Len " ++ show e)

checkExprType st (Ord e) 
  = case checkExprType st e of
    WChar -> WInt
    _ -> error ("Ord " ++ show e)

checkExprType st (Chr e) 
  = case checkExprType st e of
    WInt -> WChar
    _ -> error ("Chr " ++ show e)

checkExprType st (e1 :*: e2)
  | checkExprType st e1 == WInt && checkExprType st e2 == WInt = WInt
  | otherwise = error "* operands are not both ints"
checkExprType st (e1 :/: e2)
  | checkExprType st e1 == WInt && checkExprType st e2 == WInt = WInt
  | otherwise = error "/ operands are not both ints"
checkExprType st (e1 :%: e2)
  | checkExprType st e1 == WInt && checkExprType st e2 == WInt = WInt
  | otherwise = error "% operands are not both ints"
checkExprType st (e1 :+: e2)
  | checkExprType st e1 == WInt && checkExprType st e2 == WInt = WInt
  | otherwise = error "+ operands are not both ints"
checkExprType st (e1 :-: e2)
  | checkExprType st e1 == WInt && checkExprType st e2 == WInt = WInt
  | otherwise = error "- operands are not both ints"

checkExprType st (e1 :>: e2)
  | te1 `notElem` [WChar, WInt] = error "first operand of > not char/int"
  | te2 `notElem` [WChar, WInt] = error "second operand of > not char/int"
  | te1 == te2 = WBool
  | otherwise = error "> operands are not the same type"
  where
    te1 = checkExprType st e1
    te2 = checkExprType st e2
checkExprType st (e1 :>=: e2)
  | te1 `notElem` [WChar, WInt] = error "first operand of >= not char/int"
  | te2 `notElem` [WChar, WInt] = error "second operand of >= not char/int"
  | te1 == te2 = WBool
  | otherwise = error ">= operands are not the same type"
  where
    te1 = checkExprType st e1
    te2 = checkExprType st e2
checkExprType st (e1 :<: e2)
  | te1 `notElem` [WChar, WInt] = error "first operand of < not char/int"
  | te2 `notElem` [WChar, WInt] = error "second operand of < not char/int"
  | te1 == te2 = error (show te1 ++ show te2)
  | otherwise = error "< operands are not the same type"
  where
    te1 = checkExprType st e1
    te2 = checkExprType st e2
checkExprType st (e1 :<=: e2)
  | te1 `notElem` [WChar, WInt] = error "first operand of <= not char/int"
  | te2 `notElem` [WChar, WInt] = error "second operand of <= not char/int"
  | te1 == te2 = WBool
  | otherwise = error "<= operands are not the same type"
  where
    te1 = checkExprType st e1
    te2 = checkExprType st e2

checkExprType st (e1 :==: e2)
  | checkExprType st e1 == checkExprType st e2 = WBool
  | otherwise = error "== operands are not the same type"
checkExprType st (e1 :!=: e2)
  | checkExprType st e1 == checkExprType st e2 = WBool
  | otherwise = error "!= operands are not the same type"

checkExprType st (e1 :&&: e2)
  | checkExprType st e1 == WBool && checkExprType st e2 == WBool = WBool
  | otherwise = error "&& operands are not both bools"
checkExprType st (e1 :||: e2)
  | checkExprType st e1 == WBool && checkExprType st e2 == WBool = WBool
  | otherwise = error "|| operands are not both bools"

insertAssign :: SymbolTable -> WType -> Ident -> SymbolTable
insertAssign st (WArr wtype int) ident = M.insert ident (ArrType wtype int) st
insertAssign st (WPair wtype1 wtype2) ident = M.insert ident (PairType wtype1 wtype2) st
insertAssign st wtype ident = M.insert ident (VarType wtype) st 


insertParams :: [(WType, Ident)] -> SymbolTable -> SymbolTable
insertParams [] st = st
insertParams ((wType, ident):params) st 
  = insertParams params (M.insert ident (VarType wType) st) 


