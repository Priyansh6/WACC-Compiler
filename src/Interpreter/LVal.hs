module Interpreter.LVal (module Interpreter.LVal) where

import AST hiding (Ident, Scope)
import Control.Monad.Except (throwError)
import Interpreter.Expression (evalExpr, getArrayElem)
import Interpreter.Identifiers
import Interpreter.Type (checkType, iPairFst, iPairSnd, toWType)
import Interpreter.Utils
import Semantic.Errors (RuntimeError (..), SemanticError (..), arrayErrorType, pairErrorType)

assignLVal :: Scope -> LVal -> Value -> Interpreter ()
assignLVal _ (LIdent ident) value = updateIdent ident value
assignLVal _ (LArray (ArrayElem ident indexExprs pos)) value = do
  origArrVal <- getVarOrParam ident
  checkType pos [arrayErrorType] <$> toWType origArrVal
  indices <- mapM evalExpr indexExprs
  replaceInArray (address origArrVal) indices
  return ()
  where
    replaceInArray :: Address -> [Value] -> Interpreter Address
    replaceInArray _ [] = do
      -- wants to replace in array but no more indices, which means value is NOT BASE ELEM (so must be array)
      checkType pos [arrayErrorType] <$> toWType value
      lookupHeapArray (address value) pos
      return (address value)
    replaceInArray addr ((IInt i) : is) = do
      arrValues <- lookupHeapArray addr pos
      case splitAt (fromInteger i) arrValues of
        (before, prev : after) ->
          case prev of
            (IArr prevAddr) -> do
              -- nested array element
              newAddr <- replaceInArray prevAddr is
              if null is
                then updateValueInHeap addr (HArr (before ++ (IArr newAddr : after))) pos >> return addr
                else return addr
            _ -> do
              -- Base element
              updateValueInHeap addr (HArr (before ++ (value : after))) pos
              return addr
        _ -> throwError $ Runtime IndexOutOfBounds pos
    replaceInArray _ (i : _) = toWType i >>= throwError . IncompatibleTypes pos [WInt]

assignLVal _ (LPair (Fst lval pos)) rValue = do
  lValue <- evalLVal lval
  wtRVal <- toWType rValue
  sndOfLval <- case lValue of
    IPair addr -> do
      (left, right) <- lookupHeapPair addr pos
      wtLeft <- toWType left
      checkType pos [wtLeft] wtRVal
      return right
    IUnit -> throwError $ Runtime NullDereference pos
    v -> toWType v >>= throwError . IncompatibleTypes pos [pairErrorType]
  modifyHeapPair (address lValue) (HPair rValue sndOfLval)

assignLVal _ (LPair (Snd lval pos)) rValue = do
  lValue <- evalLVal lval
  wtRVal <- toWType rValue
  fstOfLval <- case lValue of
    IPair addr -> do
      (left, right) <- lookupHeapPair addr pos
      wtRight <- toWType right
      checkType pos [wtRight] wtRVal
      return left
    IUnit -> throwError $ Runtime NullDereference pos
    v -> toWType v >>= throwError . IncompatibleTypes pos [pairErrorType]
  modifyHeapPair (address lValue) (HPair fstOfLval rValue)

evalLVal :: LVal -> Interpreter Value
evalLVal (LIdent ident) = getVarOrParam ident
evalLVal (LArray arrElem) = getArrayElem arrElem
evalLVal (LPair (Fst lval pos)) = evalLVal lval >>= iPairFst pos
evalLVal (LPair (Snd lval pos)) = evalLVal lval >>= iPairSnd pos
