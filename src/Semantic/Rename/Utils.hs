module Semantic.Rename.Utils (module Semantic.Rename.Utils) where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bool

import AST
import Semantic.Errors

type ScopeStack = [Int]
type ScopeMap = M.Map Int [Ident]

type Renamer a = ReaderT ScopeStack (State Aux) a

data Aux = Aux
  { scopeMap :: ScopeMap,
    scopeCounter :: Int,
    errors :: [SemanticError]
  }
  deriving (Show, Eq)

initScopeStack :: ScopeStack
initScopeStack = [0]

initAux :: Aux
initAux =
  Aux
    { scopeMap = M.empty,
      scopeCounter = 0,
      errors = []
    }
    
prepareNewScope :: Renamer a -> Renamer a
prepareNewScope renamer = do
  count <- gets scopeCounter
  modify (\a -> a {scopeCounter = count + 1})
  local (count + 1 :) renamer

getCurrentScope :: Renamer Int
getCurrentScope = asks head

getScopedVars :: Int -> Renamer [Ident]
getScopedVars s = M.findWithDefault [] s <$> gets scopeMap

addScopeToIdent :: Int -> Ident -> Ident
addScopeToIdent scope (Ident i pos) =
  Ident (T.append (T.snoc i '-') (T.pack (show scope))) pos

getOriginalIdent :: Ident -> Ident
getOriginalIdent (Ident i pos) = Ident (T.takeWhile ('-' /=) i) pos

addSemanticError :: SemanticError -> Renamer ()
addSemanticError e = modify (\a@Aux {errors = es} -> a {errors = e : es})

insertIdentInScopeMap :: Int -> Ident -> Renamer ()
insertIdentInScopeMap s name = do
  sVars <- getScopedVars s
  modify (\a@Aux {scopeMap = sMap} -> a {scopeMap = M.insert s (name : sVars) sMap})

identInScope :: Int -> Ident -> Renamer Bool
identInScope s name = do
  sVars <- getScopedVars s
  return $ name `elem` sVars

identInScopeStack :: Ident -> Renamer Bool
identInScopeStack name = do
  stack <- ask
  case stack of
    [] -> return False
    (s:_) -> identInScope s name >>= bool (local tail $ identInScopeStack name) (return True)