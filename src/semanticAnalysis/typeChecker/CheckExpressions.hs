module CheckExpressions (checkExprType, getArrayElemBaseType, getArrayBaseType) where

import AST
import Control.Monad.Except
import Control.Monad.Trans.Writer ()
import SemanticErrors
import SymbolTable

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
  | t1 == t2 = return True
  | otherwise = throwError (IncompatibleTypes pos [t1] t2) >> return False

getArrayBaseType :: WType -> WType
getArrayBaseType (WArr wtype _) = getArrayBaseType wtype
getArrayBaseType wtype = wtype

getArrayElemBaseType :: ArrayElem -> ScopedSemanticAnalyser WType
getArrayElemBaseType (ArrayElem ident exprs pos) = do
  wtype <- getIdentType ident
  exprTypes <- mapM checkExprType exprs
  unless (all (== WInt) exprTypes) $ throwError $ IncompatibleTypes pos [WInt] wtype
  case wtype of
    ex@(WArr baseType dim) ->
      let dim' = dim - length exprs
       in if dim' < 0
            then throwError $ IncompatibleTypes pos [ex] (getArrayErrorType (length exprs) $ getArrayBaseType baseType)
            else
              if dim' == 0
                then return baseType
                else return $ WArr baseType (dim - length exprs)
    _ -> throwError (IncompatibleTypes pos [arrayErrorType] wtype)

isArrType :: Position -> WType -> ScopedSemanticAnalyser Bool
isArrType _ (WArr _ _) = return True
isArrType pos t = throwError (IncompatibleTypes pos [WArr WUnit 0] t) >> return False
