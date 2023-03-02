{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Expressions (transExp, transArrayElem) where

import AST hiding (Ident)
import CodeGeneration.IR
import Control.Monad.State
import CodeGeneration.Utils
  ( IRStatementGenerator,
    (<++),
    nextLabel,
    nextFreeReg,
    makeRegAvailable,
    makeRegsAvailable,
    getVarReg,
    addHelperFunc,
    Aux (..),
    typeSize )
import CodeGeneration.Helpers (HelperFunc(ArrLoad, ErrDivZero, BoundsCheck), showHelperLabel)
import Data.Char (ord)

import qualified AST (Ident(Ident))
import qualified Data.Map as M

type NumInstrCons a = Operand a -> Operand a -> Operand a -> Instr a
type BranchInstrCons a = Label -> Instr a

transExp :: Expr -> IRReg -> IRStatementGenerator IRInstrs
transExp (IntLiter x _) dst = return [Mov (Reg dst) (Imm (fromIntegral x))]
transExp (BoolLiter True _) dst
  = return [Mov (Reg dst) true]
  where
    true = Imm 1
transExp (BoolLiter False _) dst
  = return [Mov (Reg dst) false]
  where
    false = Imm 0
transExp (CharLiter c _) dst = return [Mov (Reg dst) (Imm (ord c))]
transExp (StrLiter t _) dst = (gets (\Aux {literTable = lt} -> lt M.! t)) >>= (\label -> return [Load (Reg dst) (Abs label)])
transExp (PairLiter _) dst
  = return [Mov (Reg dst) nullptr]
  where
    nullptr = Imm 0
transExp (IdentExpr (AST.Ident i _) _) dst = getVarReg (Ident i) >>= (\r -> return [Mov (Reg dst) (Reg r)])
transExp (ArrayExpr ae _) dst = transArrayElem ae dst
transExp (Not e _) dst = do
  eReg <- nextFreeReg
  exprInstrs <- transExp e eReg
  trueLabel <- nextLabel
  endLabel <- nextLabel
  makeRegAvailable eReg
  return $ exprInstrs ++ [Cmp (Reg dst) (Imm 1), Je trueLabel, Mov (Reg eReg) (Imm 1), Jmp endLabel, Define trueLabel, Mov (Reg eReg) (Imm 0), Define endLabel, Mov (Reg dst) (Reg eReg)]
transExp (Neg e _) dst = do
  eReg <- nextFreeReg
  exprInstrs <- transExp e dst
  makeRegAvailable eReg
  return $ exprInstrs ++ [Mov (Reg dst) (Imm 0), Sub (Reg dst) (Reg dst) (Reg eReg)]
transExp (Len e _) dst = transExp e dst <++ [Load (Reg dst) (ImmOffset dst (- (typeSize WInt)))]
transExp (Ord e _) dst = transExp e dst
transExp (Chr e _) dst = return []
transExp ((:*:) e e' _) dst = transNumOp Mul e e' dst
transExp ((:/:) e e' _) dst = addHelperFunc ErrDivZero >> transNumOp Div e e' dst
transExp ((:%:) e e' _) dst = addHelperFunc ErrDivZero >> transNumOp Mod e e' dst
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

transArrayElem :: ArrayElem -> IRReg -> IRStatementGenerator IRInstrs
transArrayElem (ArrayElem (AST.Ident i _) exprs _) dst = do
  addHelperFunc BoundsCheck
  varReg <- getVarReg (Ident i)
  concat <$> mapM (`transArrExpr` varReg) exprs
  where
    transArrExpr :: Expr -> IRReg -> IRStatementGenerator IRInstrs
    transArrExpr e varReg' = do
      exprReg <- nextFreeReg
      exprInstrs <- transExp e exprReg
      makeRegAvailable exprReg
      return $ exprInstrs ++ [Mov (Reg $ IRParam 0) (Reg varReg'), Mov (Reg $ IRParam 1) (Reg exprReg), Jsr $ showHelperLabel ArrLoad, Mov (Reg dst) (Reg IRRet)]