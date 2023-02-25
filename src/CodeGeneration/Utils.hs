{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Utils 
  ( IRSectionGenerator,
    IRStatementGenerator,
    Aux(Aux, available, labelId),
    nextFreeReg,
    makeRegAvailable,
    makeRegsAvailable,
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

import qualified Data.Map as M
import qualified Data.Text as T

type IRStatementGenerator a = StateT Aux (Reader (SymbolTable, ScopeMap)) a
type IRSectionGenerator a = (Reader (SymbolTable, ScopeMap)) a

data Aux = Aux { 
  available :: [IRReg],
  labelId :: Int,
  varLocs :: M.Map Ident IRReg }

nextFreeReg :: IRStatementGenerator IRReg
nextFreeReg = state (\a@Aux {available = (nxt:rst)} -> (nxt, a {available = rst}))

makeRegAvailable :: IRReg -> IRStatementGenerator ()
makeRegAvailable r = modify (\a@Aux {available = rs} -> a {available = r:rs})

makeRegsAvailable :: [IRReg] -> IRStatementGenerator ()
makeRegsAvailable = mapM_ makeRegAvailable

nextLabel :: IRStatementGenerator Label
nextLabel = nextLabelId >>= toLabel
  where
    nextLabelId :: StateT Aux (Reader (SymbolTable, ScopeMap)) Int
    nextLabelId = state (\a@Aux {labelId = l} -> (l, a {labelId = l + 1}))

    toLabel :: Int -> StateT Aux (Reader (SymbolTable, ScopeMap)) Label
    toLabel x = return $ "_L" <> T.pack (show x)

insertVarReg :: Ident -> IRReg -> IRStatementGenerator ()
insertVarReg i r = modify (\a@Aux {varLocs = vl} -> a {varLocs = M.insert i r vl})

(<++>) :: Applicative m => m [a] -> m [a] -> m [a]
a <++> b = (++) <$> a <*> b

(++>) :: Applicative m => [a] -> m [a] -> m [a]
a ++> b = (++) a <$> b

(<++) :: Applicative m => m [a] -> [a] -> m [a]
a <++ b = (++) <$> a  <*> pure b