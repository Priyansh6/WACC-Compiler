module Semantic.Type.SymbolTable (module Semantic.Type.SymbolTable) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Writer ()
import qualified Data.Map as M

import AST
import Error.Semantic (SemanticError)

type Env = Maybe WType

data IdentType =  FuncType WType [Ident]
                | VarType WType
                | ArrType WType Int
                | PairType WType WType
                deriving(Show, Eq)

fromIdentType :: IdentType -> WType
fromIdentType (FuncType wtype _ ) = wtype
fromIdentType (VarType wtype) = wtype
fromIdentType (ArrType wtype x) = WArr wtype x
fromIdentType (PairType wtype wtype') = WPair wtype wtype'

getIdentType :: Ident -> ScopedSemanticAnalyser WType
getIdentType ident = do
  st <- get
  return $ fromIdentType $ st M.! ident

type SymbolTable = M.Map Ident IdentType

type SemanticAnalyser = StateT SymbolTable (Except SemanticError)
type ScopedSemanticAnalyser = ReaderT Env (StateT SymbolTable (Except SemanticError))