{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Expression (module Interpreter.Expression) where

import AST hiding (Scope)
import Control.Monad.Except
import Data.Char
import Interpreter.Identifiers
import Interpreter.Type (checkType, toWType)
import Interpreter.Utils
import Semantic.Errors
import Data.Functor ((<&>))

evalExpr :: Expr -> Interpreter Value
evalExpr (IntLiter i _) = return $ IInt (fromInteger i)
evalExpr (BoolLiter bool _) = return $ IBool bool
evalExpr (CharLiter char _) = return $ IChar char
evalExpr (StrLiter str _) = return $ IStr str
evalExpr (PairLiter _) = return IUnit
evalExpr (IdentExpr ident _) = getVarOrParam ident
evalExpr (ArrayExpr arrElem _) = getArrayElem arrElem
evalExpr (Not expr pos) = evalUnOp (\(IBool bool) -> return $ IBool (not bool)) expr pos WBool
evalExpr (Neg expr pos) = evalUnOp (\(IInt i) -> return $ IInt (-i)) expr pos WInt
evalExpr (Len expr pos) = evalUnOp (len pos) expr pos arrayErrorType
evalExpr (Ord expr pos) = evalUnOp (\(IChar c) -> return $ IInt (toInteger (ord c))) expr pos WChar
evalExpr (Chr expr pos) = evalUnOp (\(IInt i) -> return $ IChar (chr (fromInteger i))) expr pos WInt
evalExpr ((:*:) exp1 exp2 pos) = evalNumericBinOp (*) exp1 exp2 pos
evalExpr ((:+:) exp1 exp2 pos) = evalNumericBinOp (+) exp1 exp2 pos
evalExpr ((:-:) exp1 exp2 pos) = evalNumericBinOp (-) exp1 exp2 pos
evalExpr ((:/:) exp1 exp2 pos) = evalNumericBinOp' div exp1 exp2 pos
evalExpr ((:%:) exp1 exp2 pos) = evalNumericBinOp' rem exp1 exp2 pos
evalExpr ((:>:) exp1 exp2 pos) = evalOrdBinOp (>) exp1 exp2 pos
evalExpr ((:<:) exp1 exp2 pos) = evalOrdBinOp (<) exp1 exp2 pos
evalExpr ((:>=:) exp1 exp2 pos) = evalOrdBinOp (>=) exp1 exp2 pos
evalExpr ((:<=:) exp1 exp2 pos) = evalOrdBinOp (<=) exp1 exp2 pos
evalExpr ((:==:) exp1 exp2 pos) = evalEqBinOp (==) exp1 exp2 pos
evalExpr ((:!=:) exp1 exp2 pos) = evalEqBinOp (/=) exp1 exp2 pos
evalExpr ((:&&:) exp1 exp2 pos) = evalBoolBinOp (&&) exp1 exp2 pos
evalExpr ((:||:) exp1 exp2 pos) = evalBoolBinOp (||) exp1 exp2 pos

len :: Position -> Value -> Interpreter Value
len pos (IArr addr) = lookupHeapArray addr pos <&> (IInt . toInteger . length)
len pos v = toWType v >>= throwError . IncompatibleTypes pos [arrayErrorType]

smallestWaccInt, biggestWaccInt :: Integer
biggestWaccInt = 2 ^ (31 :: Integer) - 1
smallestWaccInt = -(2 ^ (31 :: Integer))

catchOverflow :: Position -> Value -> Interpreter Value
catchOverflow pos val = case val of
  IInt i | i < smallestWaccInt -> throwError $ Runtime IntegerUnderflow pos
  IInt i | i > biggestWaccInt -> throwError $ Runtime IntegerOverflow pos
  v -> return v

evalUnOp :: (Value -> Interpreter Value) -> Expr -> Position -> WType -> Interpreter Value
evalUnOp op expr pos expected = do
  e <- evalExpr expr
  result <- catchOverflow (position expr) <$> op e
  eType <- toWType e
  case eType of
    (WArr _ _) | expected == arrayErrorType -> result
    wt | wt == expected -> result
    wt -> throwError $ IncompatibleTypes pos [expected] wt

evalNumericBinOp, evalNumericBinOp' :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Position -> Interpreter Value
evalNumericBinOp op exp1 exp2 pos =
  evalBinOp (\(IInt a) (IInt b) -> IInt (op a b)) exp1 exp2 (isValidNumericOperator pos)
evalNumericBinOp' op exp1 exp2 pos = do
  divisor <- evalExpr exp2
  if divisor == IInt 0
    then throwError $ Runtime DivideByZero pos
    else evalBinOp (\(IInt a) (IInt b) -> IInt (op a b)) exp1 exp2 (isValidNumericOperator pos)

evalEqBinOp, evalOrdBinOp :: (Value -> Value -> Bool) -> Expr -> Expr -> Position -> Interpreter Value
evalOrdBinOp op exp1 exp2 pos = evalBinOp ((IBool .) . op) exp1 exp2 (isValidOrdOperator pos)
evalEqBinOp op exp1 exp2 pos = evalBinOp ((IBool .) . op) exp1 exp2 (isValidEqOperator pos)

evalBoolBinOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Position -> Interpreter Value
evalBoolBinOp op exp1 exp2 pos = evalBinOp (\(IBool a) (IBool b) -> IBool (op a b)) exp1 exp2 (isValidBoolOperator pos)

evalBinOp :: (Value -> Value -> Value) -> Expr -> Expr -> (Value -> Value -> Interpreter Bool) -> Interpreter Value
evalBinOp op exp1 exp2 predicate = do
  e1 <- evalExpr exp1
  e2 <- evalExpr exp2
  _ <- predicate e1 e2
  catchOverflow (position exp1) (op e1 e2)

isValidNumericOperator :: Position -> Value -> Value -> Interpreter Bool
isValidNumericOperator _ (IInt _) (IInt _) = return True
isValidNumericOperator pos e1 e2 = do
  e1Type <- toWType e1
  e2Type <- toWType e2
  throwError (semanticError pos [WInt] e1Type e2Type) >> return False

isValidOrdOperator :: Position -> Value -> Value -> Interpreter Bool
isValidOrdOperator _ (IInt _) (IInt _) = return True
isValidOrdOperator _ (IChar _) (IChar _) = return True
isValidOrdOperator pos e1 e2 = do
  e1Type <- toWType e1
  e2Type <- toWType e2
  throwError (semanticError pos [WInt, WChar] e1Type e2Type) >> return False

isValidEqOperator :: Position -> Value -> Value -> Interpreter Bool
isValidEqOperator pos e1 e2 = do
  e1Type <- toWType e1
  e2Type <- toWType e2
  checkType pos [e1Type] e2Type
  return True

isValidBoolOperator :: Position -> Value -> Value -> Interpreter Bool
isValidBoolOperator _ (IBool _) (IBool _) = return True
isValidBoolOperator pos e1 e2 = do
  e1Type <- toWType e1
  e2Type <- toWType e2
  throwError (semanticError pos [WBool] e1Type e2Type) >> return False

getArrayElem :: ArrayElem -> Interpreter Value
getArrayElem (ArrayElem ident indices pos) =
  getVarOrParam ident
    >>= \case
      arr@(IArr _) -> mapM evalExpr indices >>= getArrElem arr
      _ -> throwError $ NonSubscriptable pos
  where
    getArrElem :: Value -> [Value] -> Interpreter Value
    getArrElem val [] = return val
    getArrElem (IArr addr') ((IInt i) : is) = do
      arr <- lookupHeapArray addr' pos
      if null arr
        then throwError $ Runtime IndexOutOfBounds pos
        else
          if 0 <= i && i <= toInteger (length arr) - 1
            then getArrElem (arr !! fromInteger i) is
            else throwError $ Runtime IndexOutOfBounds pos
    getArrElem (IArr _) (i : _) = toWType i >>= throwError . IncompatibleTypes pos [WInt]
    getArrElem _ _ = throwError $ NonSubscriptable pos

position :: Expr -> Position
position (IntLiter _ pos) = pos
position (BoolLiter _ pos) = pos
position (CharLiter _ pos) = pos
position (StrLiter _ pos) = pos
position (PairLiter pos) = pos
position (IdentExpr _ pos) = pos
position (ArrayExpr _ pos) = pos
position (Not _ pos) = pos
position (Neg _ pos) = pos
position (Len _ pos) = pos
position (Ord _ pos) = pos
position (Chr _ pos) = pos
position ((:*:) _ _ pos) = pos
position ((:/:) _ _ pos) = pos
position ((:%:) _ _ pos) = pos
position ((:+:) _ _ pos) = pos
position ((:-:) _ _ pos) = pos
position ((:>:) _ _ pos) = pos
position ((:>=:) _ _ pos) = pos
position ((:<:) _ _ pos) = pos
position ((:<=:) _ _ pos) = pos
position ((:==:) _ _ pos) = pos
position ((:!=:) _ _ pos) = pos
position ((:&&:) _ _ pos) = pos
position ((:||:) _ _ pos) = pos
