{-# LANGUAGE OverloadedStrings #-}

module SymbolTable (checkProg) where

import AST
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Trans.Writer
import Control.Monad.State
import Data.Map ((!))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

type SymbolTable = M.Map Ident IdentType

data IdentType =  FuncType WType [Ident] 
                | VarType WType
                | ArrType WType Int
                | PairType WType WType
                deriving(Show, Eq)

type SemanticAnalyser = StateT SymbolTable (Except T.Text)

fromIdentType :: IdentType -> WType
fromIdentType (FuncType wtype _) = wtype
fromIdentType (VarType wtype) = wtype
fromIdentType (ArrType wtype x) = WArr wtype x
fromIdentType (PairType wtype wtype') = WPair wtype wtype'

getIdentType :: Ident -> SemanticAnalyser WType
getIdentType ident = do
  st <- get 
  return $ fromIdentType $ st ! ident

isArrType :: WType -> Bool
isArrType (WArr _ _) = True
isArrType _ = False

isPairType :: WType -> Bool
isPairType (WPair _ _) = True
isPairType _ = False

checkProg :: Program -> SemanticAnalyser ()
checkProg (Program funcs stats) = checkFuncs funcs >> checkStats stats

checkFuncs :: [Func] -> SemanticAnalyser ()
checkFuncs = mapM_ checkFunc

checkFunc :: Func -> SemanticAnalyser ()
checkFunc (Func wtype ident params stats) = do
  let (_, paramIds) = unzip params
  modify (M.insert ident (FuncType wtype paramIds))  
  insertParams params
  -- Give this a reader monad with wtype
  checkStats stats
  -- Check return statements everywhere 
  return ()

checkStats :: Stats -> SemanticAnalyser ()
checkStats = mapM_ checkStat

checkStat :: Stat -> SemanticAnalyser ()
checkStat (DecAssign wtype ident rval) = do
  insertAssign wtype ident
  wtype' <- checkRVal rval
  unless (wtype == wtype') $ throwError "Declared type and assigned type do not match!" 
checkStat (Assign lval rval) = do
  ltype <- checkLVal lval
  rtype <- checkRVal rval
  when (ltype == WUnit && rtype == WUnit) $ throwError "Assigning to nested pair!"
  unless (ltype == rtype) $ throwError "Left type != right type"
checkStat (Read lval) = void $ checkLVal lval
checkStat (Free expr) = do
  wtype <- checkExprType expr
  case wtype of
    WPair _ _ -> return ()
    WArr _ _ -> return ()
    _ -> throwError "Attempt to free a non-pair or non-array"

checkStat (Return expr) = void $ checkExprType expr

checkStat (Exit expr) = do
  wtype <- checkExprType expr
  unless (wtype == WInt) $ throwError "Attempt to exit with a non integer expression"
checkStat (Print expr) = void $ checkExprType expr
checkStat (Println expr) = void $ checkExprType expr
checkStat (If expr stats1 stats2) = do
  checkStats stats1
  checkStats stats2
  wtype <- checkExprType expr
  unless (wtype == WBool) $ throwError "Expression in if statement does not evaluate to bool"
checkStat (While expr stats) = do
  checkStats stats
  wtype <- checkExprType expr
  unless (wtype == WBool) $ throwError "Expression in while statement does not evaluate to bool"
checkStat (Begin stats) = checkStats stats
checkStat Skip = return ()

checkRVal :: RVal -> SemanticAnalyser WType
checkRVal (RExpr expr) = checkExprType expr
checkRVal (ArrayLiter []) = return $ WArr WUnit 0
checkRVal (ArrayLiter exprs) = do
  wtypes <- mapM checkExprType exprs
  if all (== head wtypes) wtypes
    then return $ WArr (head wtypes) $ length wtypes
    else throwError "Types of expression do not match"

checkRVal (NewPair e1 e2) = do
  wtype1 <- checkExprType e1
  wtype2 <- checkExprType e2
  return $ WPair wtype1 wtype2

checkRVal (RPair pairElem) = checkPairElemType pairElem
checkRVal (Call ident exprs) = do 
  st <- get
  let identType = st ! ident
  case identType of
    FuncType funcType paramIdents -> do 
      exprTypes <- mapM checkExprType exprs
      paramTypes <- mapM getIdentType paramIdents
      if paramTypes == exprTypes
          then return funcType
          else throwError "Argument types do not match parameter types"
    _ -> throwError "Calling an identifier that is not a function"

checkLVal :: LVal -> SemanticAnalyser WType
checkLVal (LIdent ident) = getIdentType ident
checkLVal (LArray (ArrayElem ident exprs)) = do
  identType <- getIdentType ident
  exprTypes <- mapM checkExprType exprs
  case identType of
    WArr t _ -> if all (== identType) exprTypes
                then return t
                else throwError "Indices aren't all integers"
    _ -> throwError "Not a valid array type"

checkLVal (LPair pairElem) = checkPairElemType pairElem

checkPairElemType :: PairElem -> SemanticAnalyser WType
checkPairElemType (Fst (LIdent ident)) = do
  identType <- getIdentType ident
  case identType of
    WPair t _ -> return t
    _ -> throwError "Taking fst of a non pair type"
checkPairElemType (Fst lval@(LPair _)) = checkLVal lval
checkPairElemType (Snd (LIdent ident)) = do
  identType <- getIdentType ident
  case identType of
    WPair _ t -> return t
    _ -> throwError "Taking snd of a non pair type"
checkPairElemType (Snd lval@(LPair _)) = checkLVal lval
checkPairElemType _ = throwError "Taking fst or snd of an array" 

checkExprType :: Expr -> SemanticAnalyser WType
checkExprType (IntLiter _) = return WInt
checkExprType (BoolLiter _) = return WBool
checkExprType (CharLiter _) = return WChar
checkExprType (StrLiter _) = return WStr
checkExprType PairLiter = return $ WPair WUnit WUnit
checkExprType (IdentExpr ident) = getIdentType ident
checkExprType (ArrayExpr (ArrayElem ident _)) = getIdentType ident
checkExprType (Not expr) = checkUnOpType (== WBool) WBool expr
checkExprType (Neg expr) = checkUnOpType (== WInt) WInt expr
checkExprType (Len expr) = checkUnOpType isArrType WInt expr
checkExprType (Ord expr) = checkUnOpType (== WChar) WInt expr
checkExprType (Chr expr) = checkUnOpType (== WInt) WChar expr
checkExprType (expr :*: expr') = checkBinOpType isValidNumericOperator WInt expr expr'
checkExprType (expr :/: expr') = checkBinOpType isValidNumericOperator WInt expr expr'
checkExprType (expr :%: expr') = checkBinOpType isValidNumericOperator WInt expr expr'
checkExprType (expr :+: expr') = checkBinOpType isValidNumericOperator WInt expr expr'
checkExprType (expr :-: expr') = checkBinOpType isValidNumericOperator WInt expr expr'
checkExprType (expr :>: expr') = checkBinOpType isValidComparisonOperator WBool expr expr'
checkExprType (expr :>=: expr') = checkBinOpType isValidComparisonOperator WBool expr expr'
checkExprType (expr :<: expr') = checkBinOpType isValidComparisonOperator WBool expr expr'
checkExprType (expr :<=: expr') = checkBinOpType isValidComparisonOperator WBool expr expr'
checkExprType (expr :==: expr') = checkBinOpType (==) WBool expr expr'
checkExprType (expr :!=: expr') = checkBinOpType (==) WBool expr expr'
checkExprType (expr :&&: expr') = checkBinOpType isValidBooleanOperator WBool expr expr'
checkExprType (expr :||: expr') = checkBinOpType isValidBooleanOperator WBool expr expr'

checkUnOpType :: (WType -> Bool) -> WType -> Expr -> SemanticAnalyser WType
checkUnOpType p out expr = do
  wtype <- checkExprType expr
  unless (p wtype) $ throwError "Unary operator applied to the wrong type"
  return out

checkBinOpType :: (WType -> WType -> Bool) -> WType -> Expr -> Expr -> SemanticAnalyser WType
checkBinOpType p out expr1 expr2 = do
  wtype1 <- checkExprType expr1
  wtype2 <- checkExprType expr2
  unless (p wtype1 wtype2) $ throwError "Binary operator applied to the wrong types"
  return out

isValidNumericOperator :: WType -> WType -> Bool
isValidNumericOperator WInt WInt = True
isValidNumericOperator _ _ = False

isValidComparisonOperator :: WType -> WType -> Bool
isValidComparisonOperator WChar WChar = True
isValidComparisonOperator WInt WInt = True
isValidComparisonOperator _ _ = False

isValidBooleanOperator :: WType -> WType -> Bool
isValidBooleanOperator WBool WBool = True
isValidBooleanOperator _ _ = False

insertAssign :: WType -> Ident -> SemanticAnalyser ()
insertAssign (WArr wtype x) ident = modify $ M.insert ident (ArrType wtype x) 
insertAssign (WPair wtype wtype') ident = modify $ M.insert ident (PairType wtype wtype')
insertAssign wtype ident = modify $ M.insert ident (VarType wtype)

insertParams :: [(WType, Ident)] -> SemanticAnalyser ()
insertParams = mapM_ (uncurry insertParam)

insertParam :: WType -> Ident -> SemanticAnalyser ()
insertParam wtype ident = modify $ M.insert ident (VarType wtype)