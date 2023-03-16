{-# LANGUAGE LambdaCase #-}

module Interpreter.Identifiers (module Interpreter.Identifiers) where

import qualified AST
import Control.Monad.Except (throwError)
import Control.Monad.State (gets, modify)
import qualified Data.Map as M
import Interpreter.Utils (Aux (..), Interpreter, ReturnValue, Scope (..), Value)
import Semantic.Errors (SemanticError (..))

lookupVarOrParam :: AST.Ident -> Aux -> Maybe Value
lookupVarOrParam ident aux = case M.lookup ident (vars aux) of
  Nothing -> M.lookup ident (params aux)
  Just (_, value) -> Just value

getVarOrParam :: AST.Ident -> Interpreter Value
getVarOrParam ident =
  gets (lookupVarOrParam ident) >>= \case
    (Just val) -> return val
    _ -> throwError $ VariableNotDefined ident

addFunction :: AST.Func -> Interpreter ()
addFunction func@(AST.Func wtReturn ident ps _ _ _) = do
  prev <- gets (M.lookup ident . funcs)
  case prev of
    Nothing -> addFunction' func
    _ -> throwError $ FunctionAlreadyDefined ident wtReturn (map fst ps)

addVariable :: AST.Ident -> Scope -> Value -> Interpreter ()
addVariable ident sc v = do
  prev <- gets (M.lookup ident . vars)
  case prev of
    Nothing -> addVariable' sc ident v
    _ -> throwError $ VariableAlreadyDefined ident

addParam :: AST.Ident -> Value -> Interpreter ()
addParam ident v = do
  prev <- gets (M.lookup ident . params)
  case prev of
    Nothing -> addParameter' ident v
    _ -> throwError $ VariableAlreadyDefined ident

addFunction' :: AST.Func -> Interpreter ()
addFunction' func@(AST.Func _ ident _ _ _ _) = modify (\aux@Aux {funcs = fs} -> aux {funcs = M.insert ident func fs})

addVariable' :: Scope -> AST.Ident -> Value -> Interpreter ()
addVariable' sc ident v = modify (\aux@Aux {vars = vs} -> aux {vars = M.insert ident (sc, v) vs})

addParameter' :: AST.Ident -> Value -> Interpreter ()
addParameter' ident v = modify (\aux@Aux {params = ps} -> aux {params = M.insert ident v ps})

setReturnValue :: ReturnValue -> Interpreter ()
setReturnValue val = modify (\aux -> aux {returnValue = val})

-- modify an existing ident, error if not defined
updateIdent :: AST.Ident -> Value -> Interpreter ()
updateIdent ident value = do
  var <- gets (M.lookup ident . vars)
  case var of
    -- if defined, change value, but keep same scope
    Just (scope, _) -> addVariable' scope ident value
    -- if not a variable, check if param otherwise undefined error
    _ -> do
      param <- gets (M.lookup ident . params)
      case param of
        Just _ -> addParameter' ident value
        _ -> throwError $ VariableNotDefined ident

-- add new ident & value OR modify existing ident
addOrUpdateIdent :: AST.Ident -> Scope -> Value -> Interpreter ()
addOrUpdateIdent ident sc v = do
  prev <- gets (lookupVarOrParam ident)
  case prev of
    -- if defined, change its value but keep same scope
    (Just _) -> do
      isVariable <- gets (M.member ident . vars)
      if isVariable
        then do
          origSc <- gets (fst . (M.! ident) . vars)
          addVariable' origSc ident v
        else addParameter' ident v

    -- if not defined, add to variables
    Nothing -> addVariable' sc ident v

removeIdent :: AST.Ident -> Interpreter ()
removeIdent ident = do
  isVariable <- gets (M.member ident . vars)
  isParam <- gets (M.member ident . params)
  if isVariable
    then modify (\aux@Aux {vars = vs} -> aux {vars = M.delete ident vs})
    else
      if isParam
        then modify (\aux@Aux {params = ps} -> aux {params = M.delete ident ps})
        else error "Segmentation fault"

filterVarsByScope :: Scope -> Interpreter ()
filterVarsByScope scope =
  modify (\aux@Aux {vars = vs} -> aux {vars = M.filter ((<= scope) . fst) vs})
