module Interpreter.LVal (module Interpreter.LVal) where

import AST hiding (Ident, Scope)
import Control.Monad.Except (MonadIO (liftIO), throwError)
import Interpreter.Expression (evalExpr, getArrayElem)
import Interpreter.Identifiers
import Interpreter.Type (checkType, iPairFst, iPairSnd, toWType)
import Interpreter.Utils
import Semantic.Errors (RuntimeError (..), SemanticError (..), arrayErrorType, pairErrorType)

assignLVal :: Scope -> LVal -> Value -> Interpreter ()
assignLVal scope (LIdent ident) value = addOrUpdateIdent ident scope value
assignLVal _ (LArray (ArrayElem ident indexExprs pos)) value = do
  origArrVal <- getVarOrParam ident
  checkType pos [arrayErrorType] <$> toWType origArrVal
  indices <- mapM evalExpr indexExprs
  _ <- replaceInArray (address origArrVal) indices
  return ()
  where
    replaceInArray :: Address -> [Value] -> Interpreter Address
    replaceInArray _ [] = do
      -- wants to replace in array but no more indices, which means value is NOT BASE ELEM (so must be array)
      checkType pos [arrayErrorType] <$> toWType value
      lookupHeapArray (address value)
      return (address value)
    replaceInArray addr ((IInt i) : is) = do
      arrValues <- lookupHeapArray addr
      case splitAt (fromInteger i) arrValues of
        (before, prev : after) ->
          case prev of
            (IArr prevAddr) -> do
              -- nested array element
              newAddr <- replaceInArray prevAddr is
              if null is
                then updateValueInHeap pos addr (HArr (before ++ (IArr newAddr : after))) >> return addr
                else return addr
            _ -> do
              -- Base element
              updateValueInHeap pos addr (HArr (before ++ (value : after)))
              return addr
        _ -> throwError $ Runtime IndexOutOfBounds pos
    replaceInArray _ (i : _) = toWType i >>= throwError . IncompatibleTypes pos [WInt]
assignLVal scope (LPair (Fst lval pos)) value = do
  lValue <- evalLVal lval
  wtRVal <- toWType value
  case lValue of
    IPair _ -> do
      pair <- lookupHeap lValue
      case pair of
        (HPair left _) -> do
          wtLeft <- toWType left
          liftIO $ print lval
          liftIO $ print wtLeft
          liftIO $ print wtRVal
          checkType pos [wtLeft] wtRVal
        _ -> error "invalid pair" -- never reaches
    IUnit -> checkType pos [WUnit] wtRVal
    v -> toWType v >>= throwError . IncompatibleTypes pos [pairErrorType]
  sndOfLval <- iPairSnd pos lValue
  addHeapValue (HPair value sndOfLval) -- ??????
    >>= assignLVal scope lval
assignLVal scope (LPair (Snd lval pos)) value = do
  lValue <- evalLVal lval
  wtRVal <- toWType value
  case lValue of
    IPair _ -> do
      pair <- lookupHeap lValue
      case pair of
        (HPair _ right) -> do
          wtRight <- toWType right
          checkType pos [wtRight] wtRVal
        _ -> error "invalid pair" -- never reaches
    IUnit -> checkType pos [WUnit] wtRVal
    v -> toWType v >>= throwError . IncompatibleTypes pos [pairErrorType]
  iPairFst pos lValue
    >>= addHeapValue . flip HPair value
    >>= assignLVal scope lval

evalLVal :: LVal -> Interpreter Value
evalLVal (LIdent ident) = getVarOrParam ident
evalLVal (LArray arrElem) = getArrayElem arrElem
evalLVal (LPair (Fst lval pos)) = evalLVal lval >>= iPairFst pos
evalLVal (LPair (Snd lval pos)) = evalLVal lval >>= iPairSnd pos
