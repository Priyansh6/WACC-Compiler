module SymbolTable (module SymbolTable) where

import SemanticErrors
import AST
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Writer ()
import Control.Monad.State
import Data.Map ((!))
import qualified Data.Map as M

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
  return $ fromIdentType $ st ! ident

type SymbolTable = M.Map Ident IdentType

type SemanticAnalyser = StateT SymbolTable (Except SemanticError)
type ScopedSemanticAnalyser = ReaderT Env (StateT SymbolTable (Except SemanticError))