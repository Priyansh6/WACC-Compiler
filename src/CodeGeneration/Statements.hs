{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration.Statements (transStats) where

import AST hiding (Ident)
import CodeGeneration.IR
import CodeGeneration.Expressions (transExp)
import CodeGeneration.Utils (IRStatementGenerator, (<++), nextFreeReg, makeRegAvailable, insertVarReg, getVarReg, nextLabel, exprType, typeSize, makeRegsAvailable, getVarType)

import qualified AST (Ident(Ident))
import Semantic.Type.SymbolTable (fromIdentType)

transStats :: Stats -> IRStatementGenerator IRInstrs
transStats ss = concat <$> mapM transStat ss

transStat :: Stat -> IRStatementGenerator IRInstrs
transStat Skip = return []
transStat (DecAssign t (AST.Ident i _) r _) = do
  varReg <- nextFreeReg
  rReg <- nextFreeReg
  rInstrs <- transRVal r rReg
  makeRegAvailable rReg
  insertVarReg (Ident i) varReg
  return $ rInstrs ++ [Mov (Reg varReg) (Reg rReg)]
transStat (Assign (LIdent (AST.Ident i _)) r _) = do
  varReg <- getVarReg (Ident i)
  rReg <- nextFreeReg
  rInstrs <- transRVal r rReg
  makeRegAvailable rReg
  return $ rInstrs ++ [Mov (Reg varReg) (Reg rReg)]
transStat (Assign _ _ _) = return []
transStat (Read l _) = return []
transStat (Free e _) = return []
transStat (Return e _) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  makeRegAvailable dst
  return $ eis ++ [Mov (Reg IRRet) (Reg dst)]
transStat (Exit e _) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  makeRegAvailable dst
  return $ eis ++ [Mov (Reg IRRet) (Reg dst)]
transStat (Print e) = return []
transStat (Println e) = return []
transStat (If e ss _ ss' _ _) = do
  eReg <- nextFreeReg
  eInstrs <- transExp e eReg
  branchLabel <- nextLabel
  ssInstrs <- transStats ss
  branchLabel' <- nextLabel
  ssInstrs' <- transStats ss'
  endLabel <- nextLabel
  let condJumpInstrs = [Cmp (Reg eReg) (Imm 1), Jne branchLabel']
      branchInstrs = [Define branchLabel] ++ ssInstrs ++ [Jmp endLabel]
      branchInstrs' = Define branchLabel' : ssInstrs'
  makeRegAvailable eReg
  return $ eInstrs ++ condJumpInstrs ++ branchInstrs ++ branchInstrs' ++ [Define endLabel]
transStat (While e ss _ _) = do
  eReg <- nextFreeReg
  eInstrs <- transExp e eReg
  ssInstrs <- transStats ss
  startLabel <- nextLabel
  condLabel <- nextLabel
  makeRegAvailable eReg
  let condJumpInstrs = [Cmp (Reg eReg) (Imm 1), Je startLabel]
  return $ [Jmp condLabel, Define startLabel] ++ ssInstrs ++ [Define condLabel] ++ eInstrs ++ condJumpInstrs
transStat (Begin ss _) = transStats ss

transRVal :: RVal -> IRReg -> IRStatementGenerator IRInstrs
transRVal (RExpr e) dst = transExp e dst
transRVal (ArrayLiter es _) dst = return []
transRVal (NewPair e e' _) dst = do
  eReg <- nextFreeReg
  ePtrReg <- nextFreeReg
  eReg' <- nextFreeReg
  ePtrReg' <- nextFreeReg
  eType <- exprType e
  eType' <- exprType e'
  let pType = AST.WPair eType eType'
      movePointers = [Mov (Reg dst) (Reg IRRet), Store (Reg eReg) (ImmOffset dst 0), Store (Reg eReg') (ImmOffset dst (typeSize eType))]
  mallocFst <- transMallocCall $ typeSize eType
  mallocSnd <- transMallocCall $ typeSize eType'
  mallocPair <- transMallocCall $ typeSize pType
  evalFstInstrs <- transExp e eReg <++ mallocFst ++ [Mov (Reg ePtrReg) (Reg IRRet), Mov (Ind ePtrReg) (Reg eReg)] -- not sure if Ind as the dst of a Mov is dodgy or not
  evalSndInstrs <- transExp e' eReg' <++ mallocSnd ++ [Mov (Reg ePtrReg') (Reg IRRet), Mov (Ind ePtrReg') (Reg eReg')]
  makeRegsAvailable [eReg, eReg', ePtrReg, ePtrReg']
  return $ evalFstInstrs ++ evalSndInstrs ++ mallocPair ++ movePointers
transRVal (RPair pe) dst = transPairElem pe dst
transRVal (Call (AST.Ident i _) es _) dst = return []

-- gets the VALUE/ptr at fst pair and puts it in dst
transPairElem :: PairElem -> IRReg -> IRStatementGenerator IRInstrs 
transPairElem (Fst (LIdent (AST.Ident i _)) _) dst = getVarReg (Ident i) >>= (\r -> return [Mov (Reg dst) (Ind r)])
transPairElem (Fst (LPair pe) _) dst = transPairElem pe dst <++ [Mov (Reg dst) (Ind dst)]
transPairElem (Snd (LIdent (AST.Ident i _)) _) dst = do
  varReg <- getVarReg (Ident i) 
  aType <- getVarType (Ident i)
  return [Mov (Reg dst) (ImmOffset varReg (typeSize $ fromIdentType aType))]
transPairElem (Snd (LPair pe) _) dst = 
  transPairElem pe dst <++ [Mov (Reg dst) (ImmOffset dst $ typeSize WUnit)]
transPairElem _ _ = error "cannot take fst or snd of a non array type"

transMallocCall :: Int -> IRStatementGenerator IRInstrs
transMallocCall size = return [Mov (Reg (IRParam 0)) (Imm size), Jsr "malloc"]