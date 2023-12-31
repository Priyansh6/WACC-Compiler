module Semantic.Type.CheckRLVals (checkRVal, checkLVal, getArrayBaseType, areTypesCompatible) where 

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

import AST
import Error.Semantic (SemanticError (..), getArrayErrorType, pairErrorType)
import Semantic.Rename.Utils (addTypesToFuncIdent)
import Semantic.Type.CheckExpressions
import Semantic.Type.SymbolTable (IdentType (FuncType), ScopedSemanticAnalyser, getIdentType)

checkRVal :: RVal -> ScopedSemanticAnalyser WType
checkRVal (RExpr expr) = checkExprType expr
checkRVal (ArrayLiter [] _) = return $ WArr WUnit 1
checkRVal (ArrayLiter exprs pos) = do
  wtypes <- mapM checkExprType exprs
  if all (== head wtypes) wtypes
    then case head wtypes of
      (WArr _ dim) -> return $ WArr (getArrayBaseType (head wtypes)) (dim + 1)
      _ -> return $ WArr (head wtypes) 1
    else throwError $ IncompatibleTypes pos [head wtypes] (head (dropWhile (== head wtypes) wtypes))
checkRVal (NewPair e1 e2 _) = do
  wtype1 <- checkExprType e1
  wtype2 <- checkExprType e2
  let wtype1' = erasePairType wtype1
      wtype2' = erasePairType wtype2
  return $ WPair wtype1' wtype2'
checkRVal (RPair pairElem) = checkPairElemType pairElem
checkRVal (Call ident exprs pos) = do
  exprTypes <- mapM checkExprType exprs
  identType <- ask >>= (\(Just rT) -> gets (M.lookup (addTypesToFuncIdent ident rT exprTypes)))
  case identType of
    Nothing -> ask >>= (\(Just rT) -> throwError $ FunctionNotDefined ident rT exprTypes)
    Just (FuncType funcType paramIdents) -> do
      paramTypes <- mapM getIdentType paramIdents
      m <- mapM (\(a, b) -> areTypesCompatible a b pos) (zip exprTypes paramTypes)
      if and m
        then return funcType
        else errorByCompareParamsAndArguments paramTypes exprTypes
    _ -> error "Cannot call an ident that is not a function"
  where
    errorByCompareParamsAndArguments :: [WType] -> [WType] -> ScopedSemanticAnalyser WType
    errorByCompareParamsAndArguments ps as = throwError $ IncompatibleTypes pos [pT] aT
      where
        (pT, aT) = head (dropWhile (uncurry (==)) (zip ps as))

checkLVal :: LVal -> ScopedSemanticAnalyser WType
checkLVal (LIdent ident) = getIdentType ident
checkLVal (LArray al) = getArrayElemBaseType al
checkLVal (LPair pairElem) = checkPairElemType pairElem

erasePairType :: WType -> WType
erasePairType (WPair _ _) = WPair WUnit WUnit
erasePairType x = x

checkPairElemType :: PairElem -> ScopedSemanticAnalyser WType
checkPairElemType (Fst (LIdent ident) pos) = do
  identType <- getIdentType ident
  case identType of
    WPair t _ -> return t
    actual -> throwError $ IncompatibleTypes pos [pairErrorType] actual
checkPairElemType (Fst (LPair _) _) = return WUnit
checkPairElemType (Fst (LArray arrayElem) pos) = do
  baseType <- getArrayElemBaseType arrayElem
  case baseType of
    WPair t _ -> return t
    t -> throwError $ IncompatibleTypes pos [pairErrorType] t
checkPairElemType (Snd (LIdent ident) pos) = do
  identType <- getIdentType ident
  case identType of
    WPair _ t -> return t
    t -> throwError $ IncompatibleTypes pos [pairErrorType] t
checkPairElemType (Snd (LPair _) _) = return WUnit
checkPairElemType (Snd (LArray arrayElem) pos) = do
  baseType <- getArrayElemBaseType arrayElem
  case baseType of
    WPair _ t -> return t
    t -> throwError $ IncompatibleTypes pos [pairErrorType] t



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
areTypesCompatible ex@(WArr t dim) (WArr t' dim') pos = do {
  isC <- areTypesCompatible (getArrayBaseType t) (getArrayBaseType t') pos;
  if isC && (dim == dim')
    then return True
    else throwError typeError >> return False } `catchError` arrayCompatibilityHandler typeError
  where
   typeError = IncompatibleTypes pos [ex] (getArrayErrorType dim' $ getArrayBaseType t')
areTypesCompatible ex ac pos
  | ex == ac = return True
  | otherwise  = throwError (IncompatibleTypes pos [ex] ac) >> return False

arrayCompatibilityHandler :: SemanticError -> SemanticError -> ScopedSemanticAnalyser Bool
arrayCompatibilityHandler _ e@(IllegalPairExchange _) = throwError e
arrayCompatibilityHandler e _ = throwError e
