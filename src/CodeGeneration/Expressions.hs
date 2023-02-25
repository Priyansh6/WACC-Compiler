{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Expressions (transExp) where

import AST
import CodeGeneration.IR
import CodeGeneration.Utils (IRStatementGenerator, nextLabel, nextFreeReg, makeRegAvailable, makeRegsAvailable)
import Data.Char (ord)

type NumInstrCons a = Operand a -> Operand a -> Operand a -> Instr a
type BranchInstrCons a = Label -> Instr a

transExp :: Expr -> IRReg -> IRStatementGenerator IRInstrs
transExp (IntLiter x _) dst = return [Mov (Reg dst) (Imm (fromIntegral x))]
transExp (BoolLiter True _) dst = return [Mov (Reg dst) (Imm 1)]
transExp (BoolLiter False _) dst = return [Mov (Reg dst) (Imm 0)]
transExp (CharLiter c _) dst = return [Mov (Reg dst) (Imm (ord c))]
transExp (StrLiter t _) dst = return []
transExp (PairLiter _) dst = return []
transExp (IdentExpr (AST.Ident i _) _) dst = return []
transExp (ArrayExpr (ArrayElem (AST.Ident i _) exprs _) _) dst = return []
transExp (Not e _) dst = do
  eReg <- nextFreeReg
  exprInstrs <- transExp e eReg
  trueLabel <- nextLabel
  endLabel <- nextLabel
  makeRegAvailable eReg
  return $ exprInstrs ++ [Cmp (Reg dst) (Imm 1), Je trueLabel, Mov (Reg eReg) (Imm 1), Jmp endLabel, Define trueLabel, Mov (Reg eReg) (Imm 0), Define endLabel, Mov (Reg dst) (Reg eReg)]
transExp (Neg e _) dst = do
  exprInstrs <- transExp e dst 
  return $ exprInstrs ++ [Sub (Reg dst) (Reg dst) (Imm 0)]
transExp (Len e _) dst = return []
transExp (Ord e _) dst = transExp e dst
transExp (Chr e _) dst = return []
transExp ((:*:) e e' _) dst = transNumOp Mul e e' dst
transExp ((:/:) e e' _) dst = transNumOp Div e e' dst
transExp ((:%:) e e' _) dst = return []
transExp ((:+:) e e' _) dst = transNumOp Add e e' dst
transExp ((:-:) e e' _) dst = transNumOp Sub e e' dst
transExp ((:>:) e e' _) dst = transCmpOp Jg e e' dst
transExp ((:>=:) e e' _) dst = transCmpOp Jge e e' dst
transExp ((:<:) e e' _) dst = transCmpOp Jl e e' dst
transExp ((:<=:) e e' _) dst = transCmpOp Jle e e' dst
transExp ((:==:) e e' _) dst = transCmpOp Je e e' dst
transExp ((:!=:) e e' _) dst = transCmpOp Jne e e' dst
transExp ((:&&:) e e' _) dst = do
  cmpReg <- nextFreeReg 
  failLabel <- nextLabel
  endLabel <- nextLabel
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  let successCase = [Cmp (Reg r) (Imm 1), Jne failLabel, Cmp (Reg r') (Imm 1), Jne failLabel, Mov (Reg cmpReg) (Imm 1), Jmp endLabel]
      failCase = [Define failLabel, Mov (Reg cmpReg) (Imm 0)] 
      end = [Define endLabel, Mov (Reg dst) (Reg cmpReg)]
  makeRegsAvailable [cmpReg, r, r']
  return $ eInstrs ++ eInstrs' ++ successCase ++ failCase ++ end
transExp ((:||:) e e' _) dst = do
  cmpReg <- nextFreeReg 
  successLabel <- nextLabel
  endLabel <- nextLabel
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  let failCase = [Cmp (Reg r) (Imm 1), Jne successLabel, Cmp (Reg r') (Imm 1), Je successLabel, Mov (Reg cmpReg) (Imm 0), Jmp endLabel]
      successCase = [Define successLabel, Mov (Reg cmpReg) (Imm 1)] 
      end = [Define endLabel, Mov (Reg dst) (Reg cmpReg)]
  makeRegsAvailable [cmpReg, r, r']
  return $ eInstrs ++ eInstrs' ++ failCase ++ successCase ++ end

transNumOp :: NumInstrCons IRReg -> Expr -> Expr -> IRReg -> IRStatementGenerator IRInstrs
transNumOp cons e e' dst = do
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  makeRegsAvailable [r, r']
  return $ eInstrs ++ eInstrs' ++ [cons (Reg dst) (Reg r) (Reg r')]

transCmpOp :: BranchInstrCons IRReg -> Expr -> Expr -> IRReg -> IRStatementGenerator IRInstrs
transCmpOp cons e e' dst = do
  cmpReg <- nextFreeReg 
  greaterLabel <- nextLabel
  endLabel <- nextLabel
  let greaterCase = [Define greaterLabel, Mov (Reg cmpReg) (Imm 1)] 
      otherCase = [Mov (Reg cmpReg) (Imm 0), Jmp endLabel]
      end = [Define endLabel, Mov (Reg dst) (Reg cmpReg)]
  r <- nextFreeReg
  eInstrs <- transExp e r
  r' <- nextFreeReg
  eInstrs' <- transExp e' r'
  makeRegsAvailable [cmpReg, r, r']
  return $ eInstrs ++ eInstrs' ++ [Cmp (Reg r) (Reg r'), cons greaterLabel] ++ otherCase ++ greaterCase ++ end