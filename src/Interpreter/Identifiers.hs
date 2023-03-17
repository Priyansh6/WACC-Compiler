{-# LANGUAGE LambdaCase #-}

module Interpreter.Identifiers (module Interpreter.Identifiers) where

import qualified AST
import Control.Monad.State (gets, modify, when)
import Data.Bifunctor (second)
import qualified Data.Map as M
import Error.PrettyPrint (semanticError)
import Error.Semantic (SemanticError (..))
import Interpreter.Utils (Aux (..), Interpreter, ReturnValue, Scope (..), Value)

setReturnValue :: ReturnValue -> Interpreter ()
setReturnValue val = modify (\aux -> aux {returnValue = val})

varExists :: AST.Ident -> Aux -> Bool
varExists ident aux = M.member ident (vars aux)

-- get highest scoped value for that ident
lookupVar :: AST.Ident -> Aux -> Maybe (Scope, Value)
lookupVar ident aux =
  case M.lookup ident (vars aux) of
    Nothing -> Nothing
    Just scopeMap ->
      if M.null scopeMap
        then Nothing
        else Just (M.findMax scopeMap)

lookupVarOrParam :: AST.Ident -> Aux -> Maybe Value
lookupVarOrParam ident aux =
  case M.lookup ident (vars aux) of
    Nothing -> M.lookup ident (params aux)
    Just scopeMap -> if M.null scopeMap
      then Nothing
      else Just (snd $ M.findMax scopeMap)

getVarOrParam :: AST.Ident -> Interpreter Value
getVarOrParam ident =
  gets (lookupVarOrParam ident) >>= \case
    (Just val) -> return val
    _ -> semanticError $ VariableNotDefined ident

addFunction :: AST.Func -> Interpreter ()
addFunction func@(AST.Func wtReturn ident ps _ _ _) = do
  let funcKey = (ident, wtReturn, map fst ps)
  prev <- gets (M.lookup funcKey . funcs)
  case prev of
    Nothing -> modify (\aux@Aux {funcs = fs} -> aux {funcs = M.insert funcKey func fs})
    _ -> semanticError $ FunctionAlreadyDefined ident wtReturn (map fst ps)

addVariable :: AST.Ident -> Scope -> Value -> Interpreter ()
addVariable ident sc v = do
  mScopeValue <- gets (lookupVar ident)
  case mScopeValue of
    Nothing -> addVariable' ident sc v
    Just (origSc, _) ->
      if origSc == sc
        then semanticError $ VariableAlreadyDefined ident
        else addVariable' ident sc v

addParam :: AST.Ident -> Value -> Interpreter ()
addParam ident v = do
  prev <- gets (M.lookup ident . params)
  case prev of
    Nothing -> addParameter' ident v
    _ -> semanticError $ VariableAlreadyDefined ident

addVariable' :: AST.Ident -> Scope -> Value -> Interpreter ()
addVariable' ident sc v = do
  mScopeMap <- gets (M.lookup ident . vars)
  case mScopeMap of
    Nothing -> modify (\aux@Aux {vars = vs} -> aux {vars = M.insert ident (M.singleton sc v) vs})
    Just scopeMap -> modify (\aux@Aux {vars = vs} -> aux {vars = M.insert ident (M.insert sc v scopeMap) vs})

addParameter' :: AST.Ident -> Value -> Interpreter ()
addParameter' ident v = modify (\aux@Aux {params = ps} -> aux {params = M.insert ident v ps})

-- modify an existing ident, error if not defined
updateIdent :: AST.Ident -> Value -> Interpreter ()
updateIdent ident value = do
  prevVar <- gets (lookupVar ident)
  case prevVar of
    -- if defined and same scope, change its value in scope map
    -- if defined and diff scope, add to scope map
    (Just (origSc, origV)) -> when (origV /= value) $ addVariable' ident origSc value
    Nothing -> do
      prevParam <- gets (M.lookup ident . params)
      case prevParam of
        Just _ -> addParameter' ident value -- not variable, is param
        -- if not defined, throw error
        Nothing -> semanticError $ VariableNotDefined ident

-- add new ident & value OR modify existing ident
addOrUpdateIdent :: AST.Ident -> Scope -> Value -> Interpreter ()
addOrUpdateIdent ident sc v = do
  prevVar <- gets (lookupVar ident)
  case prevVar of
    -- if value is diff, then change in aux with whichever scope is greater
    (Just (origSc, origV)) -> when (origV /= v) $ addVariable' ident (max sc origSc) v
    Nothing -> do
      prevParam <- gets (M.lookup ident . params)
      case prevParam of
        Just _ -> addParameter' ident v -- not variable, is param
        -- if not defined, add to variables
        Nothing -> addVariable' ident sc v

removeIdent :: AST.Ident -> Interpreter ()
removeIdent ident = do
  mScopeMap <- gets (M.lookup ident . vars)
  case mScopeMap of
    Nothing -> do
      isParam <- gets (M.member ident . params)
      if isParam
        then modify (\aux@Aux {params = ps} -> aux {params = M.delete ident ps})
        else semanticError $ VariableNotDefined ident
    Just scopeMap -> modify (\aux@Aux {vars = vs} -> aux {vars = M.insert ident (M.deleteMax scopeMap) vs})

filterVarsByScope :: Scope -> Interpreter ()
filterVarsByScope scope = do
  vs <- gets (M.toList . vars)
  let vs' = map (second (M.filterWithKey (\ sc _ -> sc <= scope))) vs
  modify (\aux -> aux {vars = M.fromList vs'})
