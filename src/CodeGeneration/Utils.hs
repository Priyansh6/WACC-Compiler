{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Utils 
  ( IRSectionGenerator,
    IRStatementGenerator,
    Aux(Aux, available, labelId),
    nextFreeReg,
    nextLabel,
    (<++>),
    (++>),
    (<++),
  )
where

import CodeGeneration.IR
import Control.Monad.Reader
import Control.Monad.State
import Semantic.Type.SymbolTable
import Semantic.Rename.Scope

import qualified Data.Text as T

type IRStatementGenerator a = StateT Aux (Reader (SymbolTable, ScopeMap)) a
type IRSectionGenerator a = (Reader (SymbolTable, ScopeMap)) a

data Aux = Aux { 
  available :: [IRReg],
  labelId :: Int }

nextFreeReg :: IRStatementGenerator IRReg
nextFreeReg = do
  aux <- get
  regs <- gets available
  case regs of 
    [] -> error "no registers available!" -- we assume an infinite number of registers in our IR so should never reach this case
    (nxt:rst) -> put (aux {available = rst}) >> return nxt

nextLabel :: IRStatementGenerator Label
nextLabel = nextLabelId >>= toLabel
  where
    nextLabelId :: StateT Aux (Reader (SymbolTable, ScopeMap)) Int
    nextLabelId = do
      aux <- get
      l <- gets labelId
      put (aux {labelId = l + 1}) >> return l

    toLabel :: Int -> StateT Aux (Reader (SymbolTable, ScopeMap)) Label
    toLabel x = return $ "_L" <> T.pack (show x)

(<++>) :: Applicative m => m [a] -> m [a] -> m [a]
a <++> b = (++) <$> a <*> b

(++>) :: Applicative m => [a] -> m [a] -> m [a]
a ++> b = (++) a <$> b

(<++) :: Applicative m => m [a] -> [a] -> m [a]
a <++ b = (++) <$> a  <*> pure b