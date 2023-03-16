{-# LANGUAGE OverloadedStrings #-}

module Semantic.Rename.Utils (module Semantic.Rename.Utils) where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import AST
import Semantic.Errors

type ScopeStack = [Int]
type ScopeMap = M.Map Int (S.Set Ident)

type Renamer a = ReaderT ScopeStack (State Aux) a

data Aux = Aux
  { scopeMap :: ScopeMap,        -- Map of scope identifiers (integers) to set of variable identifiers in that scope
    funcSet :: S.Set Ident,      -- Set of function idents with parameter type information embedded into the name
    scopeCounter :: Int,         -- Highest already seen scope identifier
    errors :: [SemanticError]    -- List of semantic errors caught by the renamer
  }
  deriving (Show, Eq)

initScopeStack :: ScopeStack
initScopeStack = [0]

initAux :: Aux
initAux = Aux
    { scopeMap = M.empty,
      funcSet = S.empty,
      scopeCounter = 0,
      errors = []
    }

nextFreeScope :: Renamer Int
nextFreeScope = (+1) <$> gets scopeCounter
    
prepareNewScope :: Renamer a -> Renamer a
prepareNewScope renamer = do
  nextS <- nextFreeScope
  modify (\a -> a {scopeCounter = nextS})
  local (nextS:) renamer

getCurrentScope :: Renamer Int
getCurrentScope = asks head

getScopedVars :: Int -> Renamer (S.Set Ident)
getScopedVars s = gets scopeMap >>= return . M.findWithDefault (S.empty) s 

addScopeToIdent :: Int -> Ident -> Ident
addScopeToIdent scope (Ident i pos) =
  Ident (T.append (T.snoc i '-') (T.pack (show scope))) pos

getOriginalIdent :: Ident -> Ident
getOriginalIdent (Ident i pos) = Ident (T.takeWhile (/= '-') i) pos

addSemanticError :: SemanticError -> Renamer ()
addSemanticError e = modify (\a@Aux {errors = es} -> a {errors = e : es})

addFuncIdent :: Func -> Renamer ()
addFuncIdent f = modify (\a@Aux {funcSet = fs} -> a {funcSet = S.insert (addTypesToFunc f) fs})

funcExists :: Func -> Renamer Bool
funcExists f = gets funcSet >>= return . S.member (addTypesToFunc f)

insertIdentInScope :: Int -> Ident -> Renamer ()
insertIdentInScope s name = do
  sVars <- getScopedVars s
  modify (\a@Aux {scopeMap = sMap} -> a {scopeMap = M.insert s (S.insert name sVars) sMap})

identInScope :: Int -> Ident -> Renamer Bool
identInScope s name = getScopedVars s >>= return . S.member name

getIdentFromScopeStack :: Ident -> Renamer (Maybe Ident)
getIdentFromScopeStack name = do
  stack <- ask
  case stack of
    [] -> return Nothing
    (s:_) -> do
      let name' = addScopeToIdent s name
      declared <- identInScope s name' 
      if declared 
        then return $ Just name'
        else local tail $ getIdentFromScopeStack name

addTypesToFuncIdent :: Ident -> WType -> [WType] -> Ident
addTypesToFuncIdent (Ident i pos) rt paramTs = Ident (i <> "." <> showFuncReturnWType rt <> T.concat (map showFuncWType (paramTs))) pos

addTypesToFunc :: Func -> Ident
addTypesToFunc (Func rt name ps _ _ _) = addTypesToFuncIdent name rt (fst $ unzip ps)

getOriginalFuncIdent :: Ident -> Ident
getOriginalFuncIdent (Ident i pos) = Ident (T.takeWhile (/= '.') i) pos

showFuncWType :: WType -> T.Text
showFuncWType WUnit        = error "Cannot have a parameter with type WUnit"
showFuncWType WInt         = "i"
showFuncWType WBool        = "b"
showFuncWType WChar        = "c"
showFuncWType WStr         = "s"
showFuncWType (WArr t dim) = "a" <> T.pack (show dim) <> showFuncWType t
showFuncWType (WPair _ _)  = "p"

showFuncReturnWType :: WType -> T.Text
showFuncReturnWType (WPair WUnit WUnit) = "pair"
showFuncReturnWType (WPair t1 t2) = "pair" <> showFuncWType t1 <> showFuncWType t2
showFuncReturnWType t = showFuncWType t