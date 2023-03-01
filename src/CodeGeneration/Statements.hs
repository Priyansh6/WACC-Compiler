{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Statements (transStats) where

import Control.Monad
import Control.Monad.State
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
transStat (DecAssign _ (AST.Ident i _) r _) = do
  varReg <- nextFreeReg
  insertVarReg (Ident i) varReg
  withReg (\rReg -> transRVal r rReg <++ [Mov (Reg varReg) (Reg rReg)])
transStat (Assign l@(LIdent _) r _) = withReg (\lReg -> withReg (\rReg -> transLVal l lReg <++> transRVal r rReg <++ [Mov (Reg lReg) (Reg rReg)]))
transStat (Assign l r _) = withReg (\lReg -> withReg (\rReg -> transLVal l lReg <++> transRVal r rReg <++ [Store (Reg rReg) (Ind lReg)]))
transStat (Read l _) = do
  helperFuncType <- lValWType l <&> HRead . fromWType
  addHelperFunc helperFuncType
  withReg (\lReg -> transLVal l lReg <++ [Mov (Reg (IRParam 0)) (Reg lReg), Jsr (showHelperLabel helperFuncType)])
  where
    lValWType :: LVal -> IRStatementGenerator WType
    lValWType (LIdent (AST.Ident i _)) = getWType (Ident i)
    lValWType (LArray (ArrayElem (AST.Ident i _) _ _)) =
      getWType (Ident i) >>= (\(WArr baseType _) -> return baseType)
    lValWType (LPair (Fst lval _)) = lValWType lval >>= (\(WPair wt _) -> return wt)
    lValWType (LPair (Snd lval _)) = lValWType lval >>= (\(WPair _ wt) -> return wt)
transStat (Free e _) = do
  eType <- exprType e
  let freeInstrs errType = withReg (\refReg -> transExp e refReg <++> checkNull refReg <++ [Mov (Reg IRRet) (Reg refReg), Jsr (showHelperLabel errType)])
  case eType of
    WPair _ _ -> addHelperFunc FreePair >> freeInstrs FreePair
    WArr _ _ -> addHelperFunc FreeArr >> freeInstrs FreeArr
    _ -> undefined
transStat (Return e _) = withReg (\dst -> transExp e dst <++ [Mov (Reg IRRet) (Reg dst)])
transStat (Exit e _) = withReg (\dst -> transExp e dst <++ [Mov (Reg IRRet) (Reg dst)])
transStat (Print e) = do
  helperFuncType <- HPrint . fromWType <$> exprType e 
  eInstrs <- withReg (\r -> transExp e r <++ [Mov (Reg (IRParam 0)) (Reg r), Jsr (showHelperLabel helperFuncType)])
  addHelperFunc helperFuncType
  return eInstrs
transStat (Println e) = do
  eInstrs <- withReg (\r -> transExp e r <++ [Mov (Reg (IRParam 0)) (Reg r), Jsr (showHelperLabel HPrintln)])
  addHelperFunc HPrintln
  return eInstrs
transStat (If e ss _ ss' _ _) = do
  branchLabel <- nextLabel
  branchLabel' <- nextLabel
  endLabel <- nextLabel
  eInstrs <- withReg (\eReg -> transExp e eReg <++ [Cmp (Reg eReg) (Imm 1), Jne branchLabel'])
  ssInstrs <- transStats ss
  ssInstrs' <- transStats ss'
  let branchInstrs = [Define branchLabel] ++ ssInstrs ++ [Jmp endLabel]
      branchInstrs' = Define branchLabel' : ssInstrs'
  return $ eInstrs ++ branchInstrs ++ branchInstrs' ++ [Define endLabel]
transStat (While e ss _ _) = do
  startLabel <- nextLabel
  condLabel <- nextLabel
  eInstrs <- withReg (\eReg -> transExp e eReg <++ [Cmp (Reg eReg) (Imm 1), Je startLabel])
  ssInstrs <- transStats ss
  return $ [Jmp condLabel, Define startLabel] ++ ssInstrs ++ [Define condLabel] ++ eInstrs 
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
    transPairElem (Fst (LIdent (AST.Ident i _)) _) dst' = getVarReg (Ident i) >>= (\r -> checkNull r <++ [Load (Reg dst') (Ind r)])
    transPairElem (Fst (LPair pe') _) dst' = transPairElem pe' dst' <++> checkNull dst' <++ [Load (Reg dst') (Ind dst')]
    transPairElem (Snd (LIdent (AST.Ident i _)) _) dst' = do
      varReg <- getVarReg (Ident i) 
      aType <- getWType (Ident i)
      checkNull varReg <++ [Load (Reg dst') (ImmOffset varReg (typeSize aType))]
    transPairElem (Snd (LPair pe') _) dst' = transPairElem pe' dst' <++> checkNull dst' <++ [Load (Reg dst') (ImmOffset dst' $ typeSize WUnit)]
    transPairElem _ _ = error "cannot take fst or snd of an array type"
transRVal (Call (AST.Ident i _) es _) dst = do
  movParamInstrs <- concat <$> zipWithM transExp es [IRParam x | x <- [0..]]
  regsInUse <- gets inUse
  return $ movParamInstrs ++ [Comment "We push the dst register despite it containing uninitialised data. Thus we have to pop and then move the return register into dst.", Push (Regs regsInUse), Jsr i, Pop (Regs regsInUse), Mov (Reg dst) (Reg IRRet)] 

transLVal :: LVal -> IRReg -> IRStatementGenerator IRInstrs
transLVal (LIdent (AST.Ident i _)) dst = getVarReg (Ident i) >>= (\r -> return [Mov (Reg dst) (Reg r)])
transLVal (LPair pe) dst = withReg (\r -> transLPair pe r <++ [Mov (Reg dst) (Reg r)])
  where
    transLPair :: PairElem -> IRReg -> IRStatementGenerator IRInstrs
    transLPair (Fst (LIdent (AST.Ident i _)) _) dst' = getVarReg (Ident i) >>= (\vr -> checkNull vr <++ [Load (Reg dst') (Ind vr)])
    transLPair (Fst (LPair pe') _) dst' = withReg (\r -> transLPair pe' r <++> checkNull r <++ [Mov (Reg dst') (Ind r)])
    transLPair (Snd (LIdent (AST.Ident i _)) _) dst' = do
      vr <- getVarReg (Ident i)
      vType <- getWType (Ident i)
      checkNull vr <++ [Mov (Reg dst') (ImmOffset vr (typeSize vType))]
    transLPair (Snd (LPair pe') _) dst' = withReg (\r -> transLPair pe' r <++> checkNull dst' <++ [Mov (Reg dst') (ImmOffset r (typeSize WUnit))])
    transLPair _ _ = undefined
transLVal (LArray ae) dst = withReg (\r -> transArrayElem ae r <++ [Mov (Reg dst) (Reg r)])

transMallocCall :: Int -> IRReg -> IRStatementGenerator IRInstrs
transMallocCall size dst = return [Mov (Reg (IRParam 0)) (Imm size), Jsr "malloc", Mov (Reg dst) (Reg IRRet)]

transArrayCreation :: Int -> Int -> IRReg -> IRStatementGenerator IRInstrs
transArrayCreation size len dst = do
  sizeReg <- nextFreeReg
  mallocInstrs <- transMallocCall (typeSize WInt + size) dst
  makeRegAvailable sizeReg
  return $ mallocInstrs ++ [Mov (Reg sizeReg) (Imm len), Store (Reg sizeReg) (Ind dst), Add (Reg dst) (Reg dst) (Imm $ typeSize WInt)]

checkNull :: IRReg -> IRStatementGenerator IRInstrs
checkNull toCheck = addHelperFunc ErrNull *> (nextLabel <&> (\notNullLabel -> [Cmp (Reg toCheck) (Imm 0), Jge notNullLabel, Jsr (showHelperLabel ErrNull), Define notNullLabel]))