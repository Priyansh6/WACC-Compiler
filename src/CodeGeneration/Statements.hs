module CodeGeneration.Statements (transStats, Aux(Aux, available)) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char (ord)

import CodeGeneration.IR
import CodeGeneration.Utils ((<++>), (<++), (++>))
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)

import AST

data Aux = Aux { available :: [IRReg] }

nextFreeReg :: StateT Aux (Reader (SymbolTable, ScopeMap)) IRReg
nextFreeReg = do
  regs <- gets available
  case regs of 
    [] -> error "no registers available!" -- we assume an infinite number of registers in our IR so should never reach this case
    (nxt:rst) -> put Aux { available = rst } >> return nxt

-- Needs to carry around symbol table + scope map + offsets of any variable from fp
transStats :: Stats -> StateT Aux (Reader (SymbolTable, ScopeMap)) IRInstrs
transStats ss = concat <$> mapM transStat ss 

-- Pattern match on nodes
transStat :: Stat -> StateT Aux (Reader (SymbolTable, ScopeMap)) IRInstrs
transStat Skip = return []
transStat (DecAssign t i r _) = undefined
transStat (Assign l r _) = undefined
transStat (Read l _) = undefined
transStat (Free e _) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  return eis
transStat (Return e _) = do
  dst <- nextFreeReg 
  eis <- transExp e dst
  return $ eis ++ [Mov (Reg IRRet) (Reg dst)]
transStat (Exit e _) = do 
  dst <- nextFreeReg
  eis <- transExp e dst
  return eis
transStat (Print e) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  return eis
transStat (Println e) = do 
  dst <- nextFreeReg
  eis <- transExp e dst
  return eis
transStat (If e ss _ ss' _ _) = return [] 
transStat (While e ss _ _) = return []
transStat (Begin ss _) = transStats ss

transExp :: Expr -> IRReg -> StateT Aux (Reader (SymbolTable, ScopeMap)) IRInstrs
transExp (IntLiter x _) dst = return [Mov (Reg dst) (Imm (fromIntegral x))]
transExp (BoolLiter True _) dst = return [Mov (Reg dst) (Imm 1)]
transExp (BoolLiter False _) dst = return [Mov (Reg dst) (Imm 0)]
transExp (CharLiter c _) dst = return [Mov (Reg dst) (Imm (ord c))]
transExp _ _ = return []