module Interpreter.Type (module Interpreter.Type) where

import AST
import Control.Monad.Except (throwError, when)
import Interpreter.Utils
import Semantic.Errors (RuntimeError (..), SemanticError (..), arrayErrorType, pairErrorType)

toWType :: Value -> Interpreter WType
toWType (IInt _) = return WInt
toWType (IBool _) = return WBool
toWType (IChar _) = return WChar
toWType (IStr _) = return WStr
toWType arr@(IArr _) = do
  arr' <- lookupHeap arr
  case arr' of
    HArr [] -> return $ WArr WUnit 1 -- ???
    HArr (e : _) -> do
      base' <- toWType e
      case base' of
        (WArr b d) -> return $ WArr b (1 + d)
        _ -> return $ WArr base' 1
    _ -> error "invalid address dereference for array"
toWType p@(IPair _) = do
  pair <- lookupHeap p
  case pair of
    HPair v1 v2 -> do
      wt1 <- toWType v1
      wt2 <- toWType v2
      return $ WPair (erasePairType wt1) (erasePairType wt2)
    _ -> error "invalid address dereference for pair"
toWType IUnit = return $ WPair WUnit WUnit


checkType :: AST.Position -> [AST.WType] -> AST.WType -> Interpreter ()
checkType pos [WUnit] WUnit = throwError (IllegalPairExchange pos)
checkType _ [WUnit] _ = return ()
checkType _ _ WUnit = return ()
checkType _ [WPair WUnit WUnit] (WPair _ _) = return ()
checkType _ [WPair _ _] (WPair WUnit WUnit) = return ()
checkType pos [WPair pt1 pt2] (WPair pt1' pt2') = do
  checkType pos [pt1] pt1'
  checkType pos [pt2] pt2'
checkType pos [ex@(WPair _ _)] ac = throwError (IncompatibleTypes pos [ex] ac)
checkType _ [WStr] (WArr WChar _) = return ()
checkType _ [WArr _ _] (WArr WUnit _) = return ()
checkType _ [WArr WUnit _] (WArr _ _) = return ()
checkType pos ex@[WArr base depth] ac@(WArr base' depth') = do
  checkType pos [base] base'
  when (depth /= depth') $ throwError (IncompatibleTypes pos ex ac)
checkType pos expecteds actual =
  when (actual `notElem` expecteds) $ throwError (IncompatibleTypes pos expecteds actual)

iPairFst, iPairSnd :: AST.Position -> Value -> Interpreter Value
iPairFst pos pair@(IPair _) = do
  pair' <- lookupHeap pair
  case pair' of
    (HArr _) -> throwError $ IncompatibleTypes pos [pairErrorType] arrayErrorType
    (HPair v _) -> return v
iPairFst pos IUnit = throwError $ Runtime NullDereference pos
iPairFst p v = do
  wt <- toWType v
  throwError $ IncompatibleTypes p [pairErrorType] wt
iPairSnd pos pair@(IPair _) = do
  pair' <- lookupHeap pair
  case pair' of
    HArr _ -> throwError $ IncompatibleTypes pos [pairErrorType] arrayErrorType
    HPair _ v -> return v
iPairSnd pos IUnit = throwError $ Runtime NullDereference pos
iPairSnd p v = iPairFst p v

getArrayBaseType :: WType -> WType
getArrayBaseType (WArr wtype _) = getArrayBaseType wtype
getArrayBaseType wtype = wtype
