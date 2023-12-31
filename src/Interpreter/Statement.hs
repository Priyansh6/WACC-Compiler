{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Statement (module Interpreter.Statement) where

import AST hiding (Scope)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader
import Control.Monad.State (gets, modify)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Text.IO as TIO
import Error.PrettyPrint (runtimeError, semanticError)
import Error.Runtime (RuntimeError (..))
import Error.Semantic (SemanticError (..), arrayErrorType, pairErrorType)
import Interpreter.Expression (evalExpr, position)
import Interpreter.Identifiers
import Interpreter.LVal (assignLVal, evalLVal)
import Interpreter.Type (checkType, iPairFst, iPairSnd, toWType)
import Interpreter.Utils
import System.Exit
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

evalStatements :: Scope -> [Stat] -> Interpreter ReturnValue
evalStatements _ [] = return Nothing
evalStatements sc@(Scope isMain _) (stat : stats) = do
  evalStatement sc stat
  returnVal <- gets returnValue
  case returnVal of
    Nothing -> evalStatements sc stats
    (Just _) | isMain -> semanticError $ IllegalReturn (statPosition stat)
    _ -> return returnVal

evalFuncStatements :: [Stat] -> Interpreter ReturnValue
evalFuncStatements = evalStatements (Scope False 0)

evalStatement :: Scope -> Stat -> Interpreter ()
evalStatement _ Skip = return ()
evalStatement scope (DecAssign wt ident rval pos) = do
  value <- evalRVal scope wt rval
  actual <- toWType value
  checkType pos [wt] actual
  addVariable ident scope value
evalStatement scope (Assign lval rval pos) = do
  lValue <- evalLVal lval
  lValueType <- toWType lValue
  rValue <- evalRVal scope lValueType rval
  rValueType <- toWType rValue
  checkType pos [lValueType] rValueType
  assignLVal scope lval rValue
evalStatement scope (Read lval pos) = do
  lValue <- evalLVal lval
  case lValue of
    IUnit -> runtimeError $ NullDereference pos
    _ -> return ()
  liftIO $ hFlush stdout
  input <- liftIO getLine
  newValue <-
    evalLVal lval
      >>= ( \case
              i@(IInt _) -> case readMaybe input :: Maybe Int of
                (Just i') -> return $ IInt $ toInteger i'
                _ -> return i
              c@(IChar _) -> case input of
                (c' : _) -> return $ IChar c'
                [] -> return c
              _ -> do
                wt <- toWType lValue
                semanticError $ IncompatibleTypes pos [WInt, WChar] wt
          )
  assignLVal scope lval newValue
evalStatement _ (Free (PairLiter pos) _) = runtimeError $ NullDereference pos
evalStatement _ (Free (IdentExpr i pos) _) = do
  mVal <- gets (lookupVarOrParam i)
  case mVal of
    Just (IArr addr) -> removeIdent i >> freeHeapValue addr pos
    Just (IPair addr) -> removeIdent i >> freeHeapValue addr pos
    Just IUnit -> runtimeError $ NullDereference pos
    Just val -> do
      wt <- toWType val
      semanticError $ IncompatibleTypes pos [pairErrorType, arrayErrorType] wt
    Nothing -> semanticError $ VariableNotDefined i
evalStatement _ (Free e pos) = do
  expr <- evalExpr e
  wt <- toWType expr
  semanticError $ IncompatibleTypes pos [pairErrorType, arrayErrorType] wt
evalStatement _ (Return expr _) = evalExpr expr >>= setReturnValue . Just
evalStatement _ (Exit expr _) =
  evalExpr expr
    >>= \case
      (IInt i) -> liftIO $ exitWith (if i `mod` 256 == 0 then ExitSuccess else ExitFailure (fromInteger i `mod` 256))
      v -> do
        wt <- toWType v
        semanticError $ IncompatibleTypes (position expr) [WInt] wt
evalStatement _ (Print expr) = evalExpr expr >>= showValue >>= liftIO . TIO.putStr >> liftIO (hFlush stdout)
evalStatement _ (Println expr) = evalExpr expr >>= showValue >>= liftIO . TIO.putStrLn >> liftIO (hFlush stdout)
evalStatement scope (If expr ss1 _ ss2 _ _) =
  evalExpr expr
    >>= \case
      (IBool bool) ->
        mapM_ (evalStatement (scope + 1)) (if bool then ss1 else ss2)
          >> filterVarsByScope scope
      v -> do
        wt <- toWType v
        semanticError $ IncompatibleTypes (position expr) [WBool] wt
evalStatement scope w@(While expr ss _ _) =
  evalExpr expr
    >>= \case
      (IBool True) -> do
        returnVal <- evalStatements (scope + 1) ss
        filterVarsByScope scope
        when (isNothing returnVal) $ evalStatement scope w
      (IBool False) -> return ()
      v -> do
        wt <- toWType v
        semanticError $ IncompatibleTypes (position expr) [WBool] wt
evalStatement scope (Begin ss _) = do
  returnVal <- evalStatements (scope + 1) ss
  when (isNothing returnVal) $ filterVarsByScope scope


evalRVal :: Scope -> WType -> RVal -> Interpreter Value
evalRVal _ _ (RExpr expr) = evalExpr expr
evalRVal _ _ (ArrayLiter [] _) = addHeapValue $ HArr []
evalRVal _ _ (ArrayLiter exprs pos) = do
  elems <- mapM evalExpr exprs
  wtypes <- mapM toWType elems
  if all (== head wtypes) wtypes
    then addHeapValue $ HArr elems
    else semanticError $ IncompatibleTypes pos [head wtypes] (head (dropWhile (== head wtypes) wtypes))
evalRVal _ _ (NewPair exp1 exp2 _) = HPair <$> evalExpr exp1 <*> evalExpr exp2 >>= addHeapValue
evalRVal _ _ (RPair (Fst lval pos)) = evalLVal lval >>= iPairFst pos
evalRVal _ _ (RPair (Snd lval pos)) = evalLVal lval >>= iPairSnd pos
evalRVal _ lvalType (Call ident exprs pos) = do
  evalParams <- mapM evalExpr exprs
  wtParams <- mapM toWType evalParams
  gets (M.lookup (ident, lvalType, wtParams) . funcs)
    >>= \case
      (Just func@(Func expectedReturn _ ps _ _ _)) ->
        if length ps /= length exprs
          then semanticError $ FunctionNotDefined ident lvalType wtParams
          else do
            functions <- gets funcs
            h <- gets heap
            fAddrs <- gets freeAddresses
            result <- runInterpreter (execFunction func evalParams) $ defaultAux {funcs = functions, heap = h, freeAddresses = fAddrs}
            case result of
              Left err -> throwError err
              Right funcAux -> case returnValue funcAux of
                Nothing -> error "Function did not return a value"
                Just output -> do
                  modify (\aux -> aux {heap = heap funcAux, freeAddresses = freeAddresses funcAux})
                  wt <- toWType output
                  checkType pos [expectedReturn] wt
                  return output
      _ -> semanticError $ FunctionNotDefined ident lvalType wtParams

execFunction :: Func -> [Value] -> Interpreter ()
execFunction (Func _ _ ps ss _ pos) values = do
  zipWithM_ createParam ps values
  result <- evalFuncStatements ss
  setReturnValue result
  where
    createParam :: (WType, AST.Ident) -> Value -> Interpreter ()
    createParam (expectedWt, p) val = do
      wt <- toWType val
      checkType pos [expectedWt] wt
      isDuplicateIdent <- gets (M.member p . params)
      if isDuplicateIdent
        then semanticError $ VariableAlreadyDefined p
        else addParam p val

statPosition :: Stat -> Position
statPosition Skip = (1, 1)
statPosition (DecAssign _ _ _ pos) = pos
statPosition (Assign _ _ pos) = pos
statPosition (Read _ pos) = pos
statPosition (Free _ pos) = pos
statPosition (Return _ pos) = pos
statPosition (Exit _ pos) = pos
statPosition (Print e) = position e
statPosition (Println e) = position e
statPosition (If _ _ _ _ _ pos) = pos
statPosition (While _ _ _ pos) = pos
statPosition (Begin [] _) = (1, 1)
statPosition (Begin (s : _) _) = statPosition s
