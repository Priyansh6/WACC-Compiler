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
  trueLabel <- nextLabel
  endLabel <- nextLabel
  exprInstrs <- transExp e dst
  return $ exprInstrs ++ [Cmp (Reg dst) (Imm 1), Je trueLabel, Mov (Reg dst) (Imm 1), Jmp endLabel, Define trueLabel, Mov (Reg dst) (Imm 0), Define endLabel]
transExp (Neg e _) dst = do
  exprInstrs <- transExp e dst
  return $ exprInstrs ++ [Mov (Reg IRScratch1) (Imm 0), Sub (Reg dst) (Reg IRScratch1) (Reg dst)]
transExp (Len e _) dst = transExp e dst <++ [Load (Reg dst) (ImmOffset dst (- (typeSize WInt)))]
transExp (Ord e _) dst = transExp e dst
transExp (Chr e _) dst = return []
transExp ((:*:) e e' _) dst = transNumOp Mul e e' dst
transExp ((:/:) e e' _) dst = addHelperFunc ErrDivZero >> transNumOp Div e e' dst
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
  failLabel <- nextLabel
  endLabel <- nextLabel
  eInstrs <- transExp e dst
  eInstrs' <- transExp e' dst
  let successCase = [Pop (Reg IRScratch2), 
                     Pop (Reg IRScratch1), 
                     Cmp (Reg IRScratch1) (Imm 1), 
                     Jne failLabel, 
                     Cmp (Reg IRScratch2) (Imm 1), 
                     Jne failLabel, 
                     Mov (Reg dst) (Imm 1), Jmp endLabel]
      failCase = [Define failLabel, Mov (Reg dst) (Imm 0)]
      end = [Define endLabel]
  return $ eInstrs ++ [Push (Reg dst)] ++ eInstrs' ++ [Push (Reg dst)] ++ successCase ++ failCase ++ end
transExp ((:||:) e e' _) dst = do
  successLabel <- nextLabel
  endLabel <- nextLabel
  eInstrs <- transExp e dst
  eInstrs' <- transExp e' dst
  let failCase = [Pop (Reg IRScratch2), 
                  Pop (Reg IRScratch1), 
                  Cmp (Reg IRScratch1) (Imm 1), 
                  Jne successLabel, 
                  Cmp (Reg IRScratch2) (Imm 1), 
                  Je successLabel, 
                  Mov (Reg dst) (Imm 0), Jmp endLabel]
      successCase = [Define successLabel, Mov (Reg dst) (Imm 1)]
      end = [Define endLabel]
  return $ eInstrs ++ [Push (Reg dst)] ++ eInstrs' ++ [Push (Reg dst)] ++ failCase ++ successCase ++ end

transNumOp :: NumInstrCons IRReg -> Expr -> Expr -> IRReg -> IRStatementGenerator IRInstrs
transNumOp cons e e' dst = do
  eInstrs <- transExp e dst
  eInstrs' <- transExp e' dst
  return $ eInstrs ++ [Push (Reg dst)] ++ eInstrs' ++ [Push (Reg dst)] ++ [Pop (Reg IRScratch2), Pop (Reg IRScratch1), cons (Reg dst) (Reg IRScratch1) (Reg IRScratch2)]

transCmpOp :: BranchInstrCons IRReg -> Expr -> Expr -> IRReg -> IRStatementGenerator IRInstrs
transCmpOp cons e e' dst = do
  greaterLabel <- nextLabel
  endLabel <- nextLabel
  let greaterCase = [Define greaterLabel, Mov (Reg dst) (Imm 1)]
      otherCase = [Mov (Reg dst) (Imm 0), Jmp endLabel]
      end = [Define endLabel]
  eInstrs <- transExp e dst
  eInstrs' <- transExp e' dst
  return $ eInstrs ++ [Push (Reg dst)] ++ eInstrs' ++ [Push (Reg dst), 
                                                       Pop (Reg IRScratch2), 
                                                       Pop (Reg IRScratch1), 
                                                       Cmp (Reg IRScratch1) (Reg IRScratch2), 
                                                       cons greaterLabel] ++ otherCase ++ greaterCase ++ end

transArrayElem :: ArrayElem -> IRReg -> IRStatementGenerator IRInstrs
transArrayElem (ArrayElem (AST.Ident i _) exprs _) dst = do
  addHelperFunc BoundsCheck
  varReg <- getVarReg (Ident i)
  concat <$> mapM (`transArrExpr` varReg) exprs
  where
    transArrExpr :: Expr -> IRReg -> IRStatementGenerator IRInstrs
    transArrExpr e varReg' = transExp e dst <++ [Mov (Reg $ IRParam 0) (Reg varReg'), 
                                                 Mov (Reg $ IRParam 1) (Reg dst), 
                                                 Jsr $ showHelperLabel ArrLoad, Mov (Reg dst) (Reg IRRet)]