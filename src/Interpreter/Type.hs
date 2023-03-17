module Interpreter.Type (module Interpreter.Type) where

import AST
import Control.Monad.Except (throwError, unless, when)
import Data.Functor ((<&>))
import Error.PrettyPrint (WaccError (..))
import Error.Runtime (RuntimeError (..))
import Error.Semantic (SemanticError (..), pairErrorType)
import Interpreter.Utils

toWType :: Value -> Interpreter WType
toWType (IInt _) = return WInt
toWType (IBool _) = return WBool
toWType (IChar _) = return WChar
toWType (IStr _) = return WStr
toWType (IArr addr) = do
  arr <- lookupHeapArray addr (1, 1)
  case arr of
    [] -> return $ WArr WUnit 1 -- ???
    (e : _) -> do
      base' <- toWType e
      case base' of
        (WArr b d) -> return $ WArr b (1 + d)
        _ -> return $ WArr base' 1
toWType (IPair addr) = do
  (left, right) <- lookupHeapPair addr (1, 1)
  wt1 <- erasePairType <$> toWType left
  wt2 <- erasePairType <$> toWType right
  return $ WPair wt1 wt2
toWType IUnit = return $ WPair WUnit WUnit

erasePairType :: WType -> WType
erasePairType (WPair _ _) = WUnit
erasePairType t = t

checkType :: AST.Position -> [AST.WType] -> AST.WType -> Interpreter ()
checkType pos [WUnit] WUnit = throwError (Semantic $ IllegalPairExchange pos)
checkType _ [WUnit] _ = return ()
checkType _ _ WUnit = return ()
checkType _ [WPair WUnit WUnit] (WPair _ _) = return ()
checkType _ [WPair _ _] (WPair WUnit WUnit) = return ()
checkType pos [WPair pt1 pt2] (WPair pt1' pt2') = do
  unless
    (pt1 == pt1' && pt2 == pt2')
    (checkType pos [pt1] pt1' >> checkType pos [pt2] pt2')
checkType pos [ex@(WPair _ _)] ac = throwError (Semantic $ IncompatibleTypes pos [ex] ac)
checkType _ [WStr] (WArr WChar _) = return ()
checkType _ [WArr _ _] (WArr WUnit _) = return ()
checkType _ [WArr WUnit _] (WArr _ _) = return ()
checkType pos ex@[WArr base depth] ac@(WArr base' depth') = do
  checkType pos [base] base'
  when (depth /= depth') $ throwError (Semantic $ IncompatibleTypes pos ex ac)
checkType pos expecteds actual =
  when (actual `notElem` expecteds) $ throwError (Semantic $ IncompatibleTypes pos expecteds actual)

iPairFst, iPairSnd :: AST.Position -> Value -> Interpreter Value
iPairFst pos (IPair addr) = lookupHeapPair addr pos <&> fst
iPairFst pos IUnit = throwError $ Runtime $ NullDereference pos
iPairFst p v = toWType v >>= throwError . Semantic . IncompatibleTypes p [pairErrorType]

iPairSnd pos (IPair addr) = lookupHeapPair addr pos <&> snd
iPairSnd pos IUnit = throwError $ Runtime $ NullDereference pos
iPairSnd p v = iPairFst p v

getArrayBaseType :: WType -> WType
getArrayBaseType (WArr wtype _) = getArrayBaseType wtype
getArrayBaseType wtype = wtype
