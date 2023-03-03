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
    getWType,
    addHelperFunc,
    Aux (..),
    heapTypeSize )
import CodeGeneration.Helpers (HelperFunc(..), showHelperLabel)
import Data.Char (ord)

import qualified AST (Ident(Ident))
import qualified Data.Map as M
import qualified Data.Text as T

type NumInstrCons a = Operand a -> Operand a -> Operand a -> Instr a
type BranchInstrCons a = Label -> Instr a

transExp :: Expr -> IRReg -> IRStatementGenerator IRInstrs
transExp (IntLiter x _) dst = return [Load (Reg IRScratch1) (Abs $ T.pack $ show x), Mov (Reg dst) (Reg IRScratch1)]
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
transExp (ArrayExpr l@(ArrayElem (AST.Ident i _) es@(e:_) _) _) dst = do
  lInstrs <- transArrayElem l (IRParam 0)
  eInstrs <- transExp e (IRParam 1)
  wType <- getWType (Ident i)
  let (WArr t dim) = wType
      accType = if dim == length es then t else WArr t (dim - length es)
      tSize = heapTypeSize accType
      hf = if tSize == 1 then ArrLoadB else ArrLoad
  addHelperFunc hf
  return $ lInstrs ++ eInstrs ++ [Jsr (showHelperLabel hf), Mov (Reg dst) (Reg IRRet)]
transExp (Not e _) dst = do
  trueLabel <- nextLabel
  endLabel <- nextLabel
  exprInstrs <- transExp e dst
  return $
    exprInstrs
      ++ [ Cmp (Reg dst) (Imm 1),
           Je trueLabel,
           Mov (Reg dst) (Imm 1),
           Jmp endLabel,
           Define trueLabel,
           Mov (Reg dst) (Imm 0),
           Define endLabel
         ]
transExp (Neg e _) dst =
  addHelperFunc ErrOverflow
    >> transExp e dst
      <++ [ Mov (Reg IRScratch1) (Imm 0),
            Sub (Reg dst) (Reg IRScratch1) (Reg dst)
          ]
transExp (Len e _) dst = transExp e dst <++ [Load (Reg dst) (ImmOffset dst (-(heapTypeSize WInt)))]
transExp (Ord e _) dst = transExp e dst
transExp (Chr e _) dst = transExp e dst
transExp ((:*:) e e' _) dst = addHelperFunc ErrOverflow >> transNumOp Mul e e' dst
transExp ((:+:) e e' _) dst = addHelperFunc ErrOverflow >> transNumOp Add e e' dst
transExp ((:-:) e e' _) dst = addHelperFunc ErrOverflow >> transNumOp Sub e e' dst
transExp ((:/:) e e' _) dst = addHelperFunc ErrDivZero >> transNumOp Div e e' dst
transExp ((:%:) e e' _) dst = addHelperFunc ErrDivZero >> transNumOp Mod e e' dst
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
                  Je successLabel, 
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
  return $
    eInstrs
      ++ [Push (Reg dst)]
      ++ eInstrs'
      ++ [Push (Reg dst)]
      ++ [Pop (Reg IRScratch2), Pop (Reg IRScratch1), cons (Reg dst) (Reg IRScratch1) (Reg IRScratch2)]

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
  if length exprs > 1 then addHelperFunc ArrLoad else return ()
  varReg <- getVarReg (Ident i)
  transArrExpr varReg exprs dst
  where
    transArrExpr :: IRReg -> [Expr] -> IRReg -> IRStatementGenerator IRInstrs
    transArrExpr arrPtr [e]    arrDst = return [Mov (Reg arrDst) (Reg arrPtr)]
    transArrExpr arrPtr (e:es) arrDst = do 
      eDst <- nextFreeReg
      eInstrs <- transExp e eDst
      indexArrayInstrs <- transArrExpr arrDst es arrDst
      _ <- makeRegAvailable eDst
      return $ eInstrs 
            ++ [
                 Mov (Reg $ IRParam 0) (Reg arrPtr),
                 Mov (Reg $ IRParam 1) (Reg eDst),
                 Jsr $ showHelperLabel ArrLoad,
                 Mov (Reg arrDst) (Reg IRRet)
               ]
            ++ indexArrayInstrs