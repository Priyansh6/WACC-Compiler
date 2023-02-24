{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration.Statements (transStats, Aux(Aux, available)) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char (ord)

import AST
import CodeGeneration.IR
import CodeGeneration.Utils ((<++>), (<++), (++>))
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)

import qualified AST (Ident)

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
transStat (DecAssign t i r _) = return []
transStat (Assign l r _) = return []
transStat (Read l _) = return []
transStat (Free e _) = return []
transStat (Return e _) = do
  dst <- nextFreeReg 
  eis <- transExp e dst
  return $ eis ++ [Mov (Reg IRRet) (Reg dst)]
transStat (Exit e _) = do 
  dst <- nextFreeReg
  eis <- transExp e dst
  return $ eis ++ [Mov (Reg IRRet) (Reg dst)]
transStat (Print e) = return []
transStat (Println e) = return []
transStat (If e ss _ ss' _ _) = return [] 
transStat (While e ss _ _) = return []
transStat (Begin ss _) = transStats ss

transExp :: Expr -> IRReg -> StateT Aux (Reader (SymbolTable, ScopeMap)) IRInstrs
transExp (IntLiter x _) dst = return [Mov (Reg dst) (Imm (fromIntegral x))]
transExp (BoolLiter True _) dst = return [Mov (Reg dst) (Imm 1)]
transExp (BoolLiter False _) dst = return [Mov (Reg dst) (Imm 0)]
transExp (CharLiter c _) dst = return [Mov (Reg dst) (Imm (ord c))]
transExp (StrLiter t _) dst = return [Mov (Reg dst) (Abs "strLiter")]
transExp (PairLiter _) dst = return []
transExp (IdentExpr (AST.Ident i _) _) dst = return []
transExp (ArrayExpr (ArrayElem (AST.Ident i _) exprs _) _) dst = return []
transExp (Not e _) dst = return []
transExp (Neg e _) dst = do
  exprInstrs <- transExp e dst 
  return $ exprInstrs ++ [Sub (Reg dst) (Reg dst) (Imm 0)]
transExp (Len e _) dst = return []
transExp (Ord e _) dst = transExp e dst
transExp (Chr e _) dst = return []
transExp ((:*:) e e' _) dst = do 
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  return $ eInstrs ++ eInstrs' ++ [Mul (Reg dst) (Reg r) (Reg r')]
transExp ((:/:) e e' _) dst = do
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  return $ eInstrs ++ eInstrs' ++ [Div (Reg dst) (Reg r) (Reg r')]
transExp ((:%:) e e' _) dst = return []
transExp ((:+:) e e' _) dst = do
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  return $ eInstrs ++ eInstrs' ++ [Add (Reg dst) (Reg r) (Reg r')]
transExp ((:-:) e e' _) dst = do
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  return $ eInstrs ++ eInstrs' ++ [Sub (Reg dst) (Reg r) (Reg r')]
transExp ((:>:) e e' _) dst = return []
transExp ((:>=:) e e' _) dst = return []
transExp ((:<:) e e' _) dst = return []
transExp ((:<=:) e e' _) dst = return []
transExp ((:==:) e e' _) dst = return []
transExp ((:!=:) e e' _) dst = return []
transExp ((:&&:) e e' _) dst = return []
transExp ((:||:) e e' _) dst = return []