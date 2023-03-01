{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Statements (transStats) where

import Control.Monad
import Data.Functor ((<&>))

import AST hiding (Ident)
import qualified AST (Ident (Ident))
import CodeGeneration.Expressions (transArrayElem, transExp)
import CodeGeneration.Helpers
import CodeGeneration.IR
import CodeGeneration.Utils

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
transStat (Assign (LPair pe) r _) = do
  rReg <- nextFreeReg
  pointerReg <- nextFreeReg
  rInstrs <- transRVal r rReg
  lInstrs <- transLPair pe pointerReg
  makeRegsAvailable [rReg, pointerReg]
  return $ rInstrs ++ lInstrs ++ [Store (Reg rReg) (Ind pointerReg)]
  where
    transLPair :: PairElem -> IRReg -> IRStatementGenerator IRInstrs
    transLPair (Fst (LIdent (AST.Ident i _)) _) dst = getVarReg (Ident i )>>= (\vr -> return [Load (Reg dst) (Ind vr)])
    transLPair (Fst (LPair pe') _) dst = do
      dst' <- nextFreeReg 
      nestedInstrs <- transLPair pe' dst'
      makeRegAvailable dst'
      return $ nestedInstrs ++ [Load (Reg dst) (Ind dst')]
    transLPair (Snd (LIdent (AST.Ident i _)) _) dst = do
      vr <- getVarReg (Ident i)
      vType <- getWType (Ident i)
      return [Load (Reg dst) (ImmOffset vr (typeSize vType))]
    transLPair (Snd (LPair pe') _) dst = do
      dst' <- nextFreeReg
      nestedInstrs <- transLPair pe' dst'
      makeRegAvailable dst'
      return $ nestedInstrs ++ [Load (Reg dst) (ImmOffset dst' (typeSize WUnit))]
    transLPair _ _ = undefined
transStat (Assign (LArray ae) r _) = do
  aeReg <- nextFreeReg
  rReg <- nextFreeReg
  aeInstrs <- transArrayElem ae aeReg
  rInstrs <- transRVal r rReg
  makeRegsAvailable [aeReg, rReg]
  return $ aeInstrs ++ rInstrs ++ [Store (Reg rReg) (Ind aeReg)]
transStat (Read l _) = lvalWType l <&> (addHelperFunc . HRead . fromWType) >> return []
  where
    lvalWType :: LVal -> IRStatementGenerator WType
    lvalWType (LIdent (AST.Ident i _)) = getWType (Ident i)
    lvalWType (LArray (ArrayElem (AST.Ident i _) _ _)) =
      getWType (Ident i) >>= (\(WArr baseType _) -> return baseType)
    lvalWType (LPair (Fst lval _)) = lvalWType lval >>= (\(WPair wt _) -> return wt)
    lvalWType (LPair (Snd lval _)) = lvalWType lval >>= (\(WPair _ wt) -> return wt)
transStat (Free e _) = exprType e >>= (\(WPair {}) -> addHelperFunc FreePair) >> return []
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
transStat (Print e) = exprType e >>= (addHelperFunc . HPrint . fromWType) >> return []
transStat (Println e) = exprType e >> addHelperFunc HPrintln >> return []
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
transRVal (ArrayLiter [] _) dst = transArrayCreation 0 0 dst
transRVal (ArrayLiter elems@(e:_) _) dst = do
  eType <- exprType e
  let eSize = typeSize eType
  transArrayCreation eSize (length elems) dst <++> (concat <$> zipWithM transArrLiterElem elems [0..])
  where
    transArrLiterElem :: Expr -> Int -> IRStatementGenerator IRInstrs
    transArrLiterElem e' idx = do
      eReg <- nextFreeReg
      exprInstrs <- transExp e' eReg
      makeRegAvailable eReg
      return $ exprInstrs ++ [Mov (Reg (IRParam 0)) (Reg dst), Mov (Reg (IRParam 1)) (Imm idx), Mov (Reg (IRParam 2)) (Reg eReg), Jsr (showHelperLabel ArrStore)]
transRVal (NewPair e e' _) dst = do
  eReg <- nextFreeReg
  ePtrReg <- nextFreeReg
  eReg' <- nextFreeReg
  ePtrReg' <- nextFreeReg
  eType <- exprType e
  eType' <- exprType e'
  let pType = AST.WPair eType eType'
      movePointers = [Store (Reg eReg) (ImmOffset dst 0), Store (Reg eReg') (ImmOffset dst (typeSize eType))]
  mallocFst <- transMallocCall (typeSize eType) ePtrReg
  mallocSnd <- transMallocCall (typeSize eType) ePtrReg'
  mallocPair <- transMallocCall (typeSize pType) dst
  evalFstInstrs <- transExp e eReg <++ mallocFst ++ [Mov (Ind ePtrReg) (Reg eReg)] -- not sure if Ind as the dst of a Mov is dodgy or not
  evalSndInstrs <- transExp e' eReg' <++ mallocSnd ++ [Mov (Ind ePtrReg') (Reg eReg')]
  makeRegsAvailable [eReg, eReg', ePtrReg, ePtrReg']
  return $ evalFstInstrs ++ evalSndInstrs ++ mallocPair ++ movePointers
transRVal (RPair pe) dst = transPairElem pe dst
  where 
    -- gets the VALUE/ptr at fst pair and puts it in dst'
    transPairElem :: PairElem -> IRReg -> IRStatementGenerator IRInstrs 
    transPairElem (Fst (LIdent (AST.Ident i _)) _) dst' = getVarReg (Ident i) >>= (\r -> return [Load (Reg dst') (Ind r)])
    transPairElem (Fst (LPair pe') _) dst' = transPairElem pe' dst' <++ [Load (Reg dst') (Ind dst')]
    transPairElem (Snd (LIdent (AST.Ident i _)) _) dst' = do
      varReg <- getVarReg (Ident i) 
      aType <- getWType (Ident i)
      return [Load (Reg dst') (ImmOffset varReg (typeSize aType))]
    transPairElem (Snd (LPair pe') _) dst' = transPairElem pe' dst' <++ [Load (Reg dst') (ImmOffset dst' $ typeSize WUnit)]
    transPairElem _ _ = error "cannot take fst or snd of an array type"
transRVal (Call (AST.Ident i _) es _) dst = return []

transMallocCall :: Int -> IRReg -> IRStatementGenerator IRInstrs
transMallocCall size dst = return [Mov (Reg (IRParam 0)) (Imm size), Jsr "malloc", Mov (Reg dst) (Reg IRRet)]

transArrayCreation :: Int -> Int -> IRReg -> IRStatementGenerator IRInstrs
transArrayCreation size len dst = do
  sizeReg <- nextFreeReg
  mallocInstrs <- transMallocCall (typeSize WInt + size) dst
  makeRegAvailable sizeReg
  return $ mallocInstrs ++ [Mov (Reg sizeReg) (Imm len), Store (Reg sizeReg) (Ind dst), Add (Reg dst) (Reg dst) (Imm $ typeSize WInt)]