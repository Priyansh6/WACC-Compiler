{-# LANGUAGE OverloadedStrings #-}

module SymbolTable (checkProg) where

import AST
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Writer ()
import Control.Monad.State
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Text as T
import SemanticErrors

-------------------
type Env = Maybe WType
-------------------

type SymbolTable = M.Map Ident IdentType

data IdentType =  FuncType WType [Ident]
                | VarType WType
                | ParamType WType
                | ArrType WType Int
                | PairType WType WType
                deriving(Show, Eq)

type SemanticAnalyser = StateT SymbolTable (Except SemanticError)
type ScopedSemanticAnalyser = ReaderT Env (StateT SymbolTable (Except SemanticError))

fromIdentType :: IdentType -> WType
fromIdentType (FuncType wtype _ ) = wtype
fromIdentType (VarType wtype) = wtype
fromIdentType (ArrType wtype x) = WArr wtype x
fromIdentType (PairType wtype wtype') = WPair wtype wtype'

getIdentType :: Ident -> ScopedSemanticAnalyser WType
getIdentType ident = do
  st <- get
  return $ fromIdentType $ st ! ident

isArrType :: Position -> WType -> ScopedSemanticAnalyser Bool
isArrType _ (WArr _ _) = return True
isArrType pos t = throwError (IncompatibleTypes pos [WArr WUnit 0] t) >> return False

isPairType :: WType -> Bool
isPairType (WPair _ _) = True
isPairType _ = False

checkProg :: Program -> SemanticAnalyser ()
checkProg (Program funcs stats) = addFuncsToSymbolTable funcs >> checkFuncs funcs >> runReaderT (checkStats stats) Nothing

checkFuncs :: [Func] -> SemanticAnalyser ()
checkFuncs = mapM_ checkFunc

addFuncsToSymbolTable :: [Func] -> SemanticAnalyser ()
addFuncsToSymbolTable = mapM_ addFuncToSymbolTable

addFuncToSymbolTable :: Func -> SemanticAnalyser ()
addFuncToSymbolTable (Func wtype ident params _ _) = do
  let (_, paramIds) = unzip params
  modify (M.insert ident (FuncType wtype paramIds))
  insertParams params

checkFunc :: Func -> SemanticAnalyser ()
checkFunc (Func wtype ident params stats pos) = runReaderT (checkStats stats) (Just wtype)

checkStats :: Stats -> ScopedSemanticAnalyser ()
checkStats = mapM_ checkStat

checkStat :: Stat -> ScopedSemanticAnalyser ()
checkStat (DecAssign ltype ident rval pos) = do
  insertAssign ltype ident
  rtype <- checkRVal rval
  _ <- areTypesCompatible ltype rtype pos
  return ()
checkStat (Assign lval rval pos) = do
  ltype <- checkLVal lval
  rtype <- checkRVal rval
  _ <- areTypesCompatible ltype rtype pos
  return ()
checkStat (Read lval pos) = do
  wtype <- checkLVal lval
  case wtype of
    WInt -> return ()
    WChar -> return ()
    _ -> void $ throwError $ IncompatibleTypes pos [WInt, WChar] wtype
checkStat (Free expr pos) = do
  wtype <- checkExprType expr
  case wtype of
    WPair _ _ -> return ()
    WArr _ _ -> return ()
    _ -> throwError $ IncompatibleTypes pos [WPair (WPair WUnit WUnit) (WPair WUnit WUnit), WArr WUnit 0] wtype
checkStat (Return expr pos) = do
  rtype <- checkExprType expr
  ftype <- ask
  _ <- case ftype of
    Nothing -> throwError $ IllegalReturn pos
    Just wtype -> areTypesCompatible wtype rtype pos
  return ()
checkStat (Exit expr pos) = do
  wtype <- checkExprType expr
  unless (wtype == WInt) $ throwError $ IncompatibleTypes pos [WInt] wtype
checkStat (Print expr) = void $ checkExprType expr
checkStat (Println expr) = void $ checkExprType expr
checkStat (If expr stats1 stats2 pos) = do
  checkStats stats1
  checkStats stats2
  wtype <- checkExprType expr
  unless (wtype == WBool) $ throwError $ IncompatibleTypes pos [WBool] wtype
checkStat (While expr stats pos) = do
  checkStats stats
  wtype <- checkExprType expr
  unless (wtype == WBool) $ throwError $ IncompatibleTypes pos [WBool] wtype
checkStat (Begin stats) = checkStats stats
checkStat Skip = return ()

checkRVal :: RVal -> ScopedSemanticAnalyser WType
checkRVal (RExpr expr) = checkExprType expr
checkRVal (ArrayLiter [] pos) = return $ WArr WUnit 1
checkRVal (ArrayLiter exprs pos) = do
  wtypes <- mapM checkExprType exprs
  if all (== head wtypes) wtypes
    then case head wtypes of
      (WArr _ dim) -> return $ WArr (getArrayBaseType (head wtypes)) (dim + 1)
      _ -> return $ WArr (head wtypes) 1
    else throwError $ IncompatibleTypes pos [head wtypes] (head (dropWhile (== head wtypes) wtypes))
checkRVal (NewPair e1 e2 pos) = do
  wtype1 <- checkExprType e1
  wtype2 <- checkExprType e2
  let wtype1' = erasePairType wtype1
      wtype2' = erasePairType wtype2
  return $ WPair wtype1' wtype2'
checkRVal (RPair pairElem) = checkPairElemType pairElem
checkRVal (Call ident exprs pos) = do
  st <- get
  let identType = st ! ident
  case identType of
    FuncType funcType paramIdents -> do
      exprTypes <- mapM checkExprType exprs
      paramTypes <- mapM getIdentType paramIdents
      -- if paramTypes == exprTypes
      m <- mapM (\(a, b) -> areTypesCompatible a b pos) (zip exprTypes paramTypes)
      if length exprTypes == length paramTypes && and m
          then return funcType
          else compareParamsAndArguments ident paramTypes exprTypes pos
    _ -> throwError $ FunctionNotDefined ident

compareParamsAndArguments :: Ident -> [WType] -> [WType] -> Position -> ScopedSemanticAnalyser WType
compareParamsAndArguments ident ps as pos
  | length ps /= length as = throwError (WrongArguments pos ident (length ps) (length as))
  | otherwise              = throwError $ IncompatibleTypes pos [pT] aT
  where
    (pT, aT) = head (dropWhile (uncurry (==)) (zip ps as))

erasePairType :: WType -> WType
erasePairType (WPair _ _) = WPair WUnit WUnit
erasePairType x = x

checkLVal :: LVal -> ScopedSemanticAnalyser WType
checkLVal (LIdent ident) = getIdentType ident
checkLVal (LArray al) = getArrayElemBaseType al
checkLVal (LPair pairElem) = checkPairElemType pairElem

-- arePairTypesCompatible :: WType -> WType -> Bool
-- arePairTypesCompatible WUnit WUnit = False
-- arePairTypesCompatible WUnit t2
--   | not $ isPairType t2 = True
--   | otherwise = False
-- arePairTypesCompatible t1 WUnit
--   | not $ isPairType t1 = True
--   | otherwise = False
-- arePairTypesCompatible t1 t2 = True

checkPairElemType :: PairElem -> ScopedSemanticAnalyser WType
checkPairElemType (Fst (LIdent ident) pos) = do
  identType <- getIdentType ident
  case identType of
    WPair t _ -> return t
    actual -> throwError $ IncompatibleTypes pos [WPair (WPair WUnit WUnit) (WPair WUnit WUnit)] actual
checkPairElemType (Fst lval@(LPair _) pos) = return WUnit
checkPairElemType (Fst (LArray arrayElem) pos) = do
  baseType <- getArrayElemBaseType arrayElem
  case baseType of
    WPair t _ -> return t
    t -> throwError $ IncompatibleTypes pos [WPair (WPair WUnit WUnit) (WPair WUnit WUnit)] t
checkPairElemType (Snd (LIdent ident) pos) = do
  identType <- getIdentType ident
  case identType of
    WPair _ t -> return t
    t -> throwError $ IncompatibleTypes pos [WPair (WPair WUnit WUnit) (WPair WUnit WUnit)] t
checkPairElemType (Snd lval@(LPair _) _) = return WUnit
checkPairElemType (Snd (LArray arrayElem) pos) = do
  baseType <- getArrayElemBaseType arrayElem
  case baseType of
    WPair _ t -> return t
    t -> throwError $ IncompatibleTypes pos [WPair (WPair WUnit WUnit) (WPair WUnit WUnit)] t


checkExprType :: Expr -> ScopedSemanticAnalyser WType
checkExprType (IntLiter _ _) = return WInt
checkExprType (BoolLiter _ _) = return WBool
checkExprType (CharLiter _ _) = return WChar
checkExprType (StrLiter _ _) = return WStr
checkExprType (PairLiter _) = return $ WPair WUnit WUnit
checkExprType (IdentExpr ident _) = getIdentType ident
checkExprType (ArrayExpr al _) = getArrayElemBaseType al
checkExprType (Not expr pos) = checkUnOpType (isValidEquality pos WBool) WBool expr
checkExprType (Neg expr pos) = checkUnOpType (isValidEquality pos WInt) WInt expr
checkExprType (Len expr pos) = checkUnOpType (isArrType pos) WInt expr
checkExprType (Ord expr pos) = checkUnOpType (isValidEquality pos WChar) WInt expr
checkExprType (Chr expr pos) = checkUnOpType (isValidEquality pos WInt) WChar expr
checkExprType ((:*:) expr expr' pos) = checkBinOpType (isValidNumericOperator pos) WInt expr expr'
checkExprType ((:/:) expr expr' pos) = checkBinOpType (isValidNumericOperator pos) WInt expr expr'
checkExprType ((:%:) expr expr' pos) = checkBinOpType (isValidNumericOperator pos) WInt expr expr'
checkExprType ((:+:) expr expr' pos) = checkBinOpType (isValidNumericOperator pos) WInt expr expr'
checkExprType ((:-:) expr expr' pos) = checkBinOpType (isValidNumericOperator pos) WInt expr expr'
checkExprType ((:>:) expr expr' pos) = checkBinOpType (isValidComparisonOperator pos) WBool expr expr'
checkExprType ((:<:) expr expr' pos) = checkBinOpType (isValidComparisonOperator pos) WBool expr expr'
checkExprType ((:>=:) expr expr' pos) = checkBinOpType (isValidComparisonOperator pos) WBool expr expr'
checkExprType ((:<=:) expr expr' pos) = checkBinOpType (isValidComparisonOperator pos) WBool expr expr'
checkExprType ((:==:) expr expr' pos) = checkBinOpType (isValidEquality pos) WBool expr expr'
checkExprType ((:!=:) expr expr' pos) = checkBinOpType (isValidEquality pos) WBool expr expr'
checkExprType ((:&&:) expr expr' pos) = checkBinOpType (isValidBooleanOperator pos) WBool expr expr'
checkExprType ((:||:) expr expr' pos) = checkBinOpType (isValidBooleanOperator pos) WBool expr expr'

checkUnOpType :: (WType -> ScopedSemanticAnalyser Bool) -> WType -> Expr -> ScopedSemanticAnalyser WType
checkUnOpType p out expr = do
  wtype <- checkExprType expr
  _ <- p wtype
  return out

checkBinOpType :: (WType -> WType -> ScopedSemanticAnalyser Bool) -> WType -> Expr -> Expr -> ScopedSemanticAnalyser WType
checkBinOpType p out expr1 expr2 = do
  wtype1 <- checkExprType expr1
  wtype2 <- checkExprType expr2
  _ <- p wtype1 wtype2
  return out

semanticError :: Position -> [WType] -> WType -> WType -> SemanticError
semanticError pos validTypes t1 t2 = IncompatibleTypes pos validTypes $ if t1 `elem` validTypes then t2 else t1

isValidNumericOperator :: Position -> WType -> WType -> ScopedSemanticAnalyser Bool
isValidNumericOperator _ WInt WInt = return True
isValidNumericOperator pos t1 t2 = throwError (semanticError pos [WInt] t1 t2) >> return False

isValidComparisonOperator :: Position -> WType -> WType -> ScopedSemanticAnalyser Bool
isValidComparisonOperator _ WChar WChar = return True
isValidComparisonOperator _ WInt WInt = return True
isValidComparisonOperator pos t1 t2 = throwError (semanticError pos [WInt, WChar] t1 t2) >> return False

isValidBooleanOperator :: Position -> WType -> WType -> ScopedSemanticAnalyser Bool
isValidBooleanOperator _ WBool WBool = return True
isValidBooleanOperator pos t1 t2 = throwError (semanticError pos [WBool] t1 t2) >> return False

isValidEquality :: Position -> WType -> WType -> ScopedSemanticAnalyser Bool
isValidEquality _ (WPair _ _) (WPair _ _) = return True
isValidEquality pos t1 t2
  | t1 == t2  = return True
  | otherwise = throwError (IncompatibleTypes pos [t1] t2) >> return False


insertAssign :: WType -> Ident -> ScopedSemanticAnalyser ()
insertAssign (WArr wtype x) ident = modify $ M.insert ident (ArrType wtype x)
insertAssign (WPair wtype wtype') ident = modify $ M.insert ident (PairType wtype wtype')
insertAssign wtype ident = modify $ M.insert ident (VarType wtype)

insertParams :: [(WType, Ident)] -> SemanticAnalyser ()
insertParams = mapM_ (uncurry insertParam)

insertParam :: WType -> Ident -> SemanticAnalyser ()
insertParam wtype ident = modify $ M.insert ident (VarType wtype)

areTypesCompatible :: WType -> WType -> Position -> ScopedSemanticAnalyser Bool
areTypesCompatible WUnit WUnit pos = throwError (IllegalPairExchange pos) >> return False
areTypesCompatible WUnit _ _ = return True
areTypesCompatible _ WUnit _ = return True
areTypesCompatible (WPair WUnit WUnit) (WPair WUnit WUnit) _ = return True
areTypesCompatible (WPair pt1 pt2) (WPair pt1' pt2') pos = do
  validP1 <- areTypesCompatible pt1 pt1' pos
  validP2 <- areTypesCompatible pt2 pt2' pos
  return $ validP1 && validP2
areTypesCompatible ex@(WPair _ _) ac pos = throwError (IncompatibleTypes pos [ex] ac) >> return False
areTypesCompatible WStr (WArr WChar _) _ = return True
areTypesCompatible ex@(WArr t dim) ac@(WArr t' dim') pos = do
  { isC <- areTypesCompatible (getArrayBaseType t) (getArrayBaseType t') pos;
    if isC && (dim == dim')
      then return True
      else throwTypeError >> return False } `catchError` (const throwTypeError)
    where
      throwTypeError = throwError $ IncompatibleTypes pos [ex] ac
areTypesCompatible ex ac pos
  | ex == ac = return True
  | otherwise  = throwError (IncompatibleTypes pos [ex] ac) >> return False

getArrayElemBaseType :: ArrayElem -> ScopedSemanticAnalyser WType
getArrayElemBaseType (ArrayElem ident exprs pos) = do
  wtype <- getIdentType ident
  exprTypes <- mapM checkExprType exprs
  unless (all (== WInt) exprTypes) $ throwError $ IncompatibleTypes pos [WInt] wtype
  case wtype of
    (WArr baseType dim) -> let dim' = dim - length exprs in
                            if dim' < 0
                              then throwError $ IncompatibleTypes pos [WArr baseType (length exprs)] wtype
                              else if dim' == 0
                                then return baseType
                                else return $ WArr baseType (dim - length exprs)
    _ -> throwError (IncompatibleTypes pos [WArr WUnit 0] wtype)

getArrayBaseType :: WType -> WType
getArrayBaseType (WArr wtype _) = getArrayBaseType wtype
getArrayBaseType wtype = wtype
