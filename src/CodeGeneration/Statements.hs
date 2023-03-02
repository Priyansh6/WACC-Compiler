{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Statements (transStats) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Functor ((<&>))
import Data.Maybe

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
transStat (Assign l@(LIdent (AST.Ident i _)) r _) = do
  varReg <- getVarReg (Ident i)
  withReg (\rReg -> transRVal r rReg <++ [Mov (Reg varReg) (Reg rReg)])
transStat (Assign l r _) = withReg (\lReg -> withReg (\rReg -> transLVal l lReg <++> transRVal r rReg <++ [Store (Reg rReg) (Ind lReg)]))
transStat (Read l@(LIdent (AST.Ident i _)) _) = do
  varReg <- getVarReg (Ident i)
  helperFuncType <- lValWType l <&> HRead . fromWType
  addHelperFunc helperFuncType
  return [Mov (Reg (IRParam 0)) (Reg varReg), Jsr (showHelperLabel helperFuncType), Mov (Reg varReg) (Reg IRRet)]
transStat (Read l _) = do
  helperFuncType <- lValWType l <&> HRead . fromWType
  addHelperFunc helperFuncType
  withReg (\lReg -> transLVal l lReg <++ [Mov (Reg (IRParam 0)) (Reg lReg), Jsr (showHelperLabel helperFuncType), Mov (Reg lReg) (Reg IRRet)])
transStat (Free e _) = do
  refReg <- nextFreeReg
  evalRefInstrs <- transExp e refReg
  eType <- exprType e
  makeRegAvailable refReg
  case eType of
    WPair _ _ -> addHelperFunc FreePair >> evalRefInstrs ++> checkNull refReg <++ [Mov (Reg IRRet) (Reg refReg), Jsr (showHelperLabel FreePair)]
    WArr _ _ -> addHelperFunc FreeArr >> return (evalRefInstrs ++ [Mov (Reg IRRet) (Reg refReg), Jsr (showHelperLabel FreeArr)])
    _ -> undefined
transStat (Return e _) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  makeRegAvailable dst
  return $ eis ++ [Mov (Reg IRRet) (Reg dst)]
transStat (Exit e _) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  makeRegAvailable dst
  return $ eis ++ [Mov (Reg $ IRParam 0) (Reg dst), Jsr "exit"]
transStat (Print e) = do
  helperFuncType <- HPrint . fromWType <$> exprType e 
  eInstrs <- withReg (\r -> transExp e r <++ [Mov (Reg (IRParam 0)) (Reg r), Jsr (showHelperLabel helperFuncType), Comment "end of print"])
  addHelperFunc helperFuncType
  return eInstrs
transStat (Println e) = do
  helperFuncType <- HPrint . fromWType <$> exprType e 
  eInstrs <- withReg (\r -> transExp e r <++ [Mov (Reg (IRParam 0)) (Reg r), Jsr (showHelperLabel helperFuncType), Jsr (showHelperLabel HPrintln)])
  addHelperFunc helperFuncType
  addHelperFunc HPrintln
  return eInstrs
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
      branchInstrs' = [Define branchLabel'] ++ ssInstrs'
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

-- transRVal translates rval and puts the VALUE into dst
transRVal :: RVal -> IRReg -> IRStatementGenerator IRInstrs
transRVal (RExpr e) dst = transExp e dst
transRVal (ArrayLiter [] _) dst = transArrayCreation 0 0 dst
transRVal (ArrayLiter elems@(e:_) _) dst = do
  eType <- exprType e
  let eSize = heapTypeSize eType
      helperFunc = if eSize == 1 then ArrStoreB else ArrStore
  addHelperFunc helperFunc
  transArrayCreation eSize (length elems) dst <++> (concat <$> zipWithM (transArrLiterElem helperFunc) elems [0..])
  where
    transArrLiterElem :: HelperFunc -> Expr -> Int -> IRStatementGenerator IRInstrs
    transArrLiterElem hf e' idx = do
      eReg <- nextFreeReg
      exprInstrs <- transExp e' eReg
      makeRegAvailable eReg
      return $ exprInstrs ++ [Mov (Reg (IRParam 0)) (Reg dst), Mov (Reg (IRParam 1)) (Imm idx), Mov (Reg (IRParam 2)) (Reg eReg), Jsr (showHelperLabel hf)]
transRVal (NewPair e e' _) dst = do
  eReg <- nextFreeReg
  ePtrReg <- nextFreeReg
  eReg' <- nextFreeReg
  ePtrReg' <- nextFreeReg
  eType <- exprType e
  eType' <- exprType e'
  let pType = AST.WPair eType eType'
      movePointers = [Store (Reg ePtrReg) (ImmOffset dst 0), Store (Reg ePtrReg') (ImmOffset dst (heapTypeSize eType))]
  mallocFst <- transMallocCall (heapTypeSize eType) ePtrReg
  mallocSnd <- transMallocCall (heapTypeSize eType') ePtrReg'
  mallocPair <- transMallocCall (heapTypeSize pType) dst
  evalFstInstrs <- transExp e eReg <++ mallocFst ++ [Store (Reg eReg) (Ind ePtrReg)] -- not sure if Ind as the dst of a Mov is dodgy or not
  evalSndInstrs <- transExp e' eReg' <++ mallocSnd ++ [Store (Reg eReg') (Ind ePtrReg')]
  makeRegsAvailable [eReg, eReg', ePtrReg, ePtrReg']
  return $ evalFstInstrs ++ evalSndInstrs ++ mallocPair ++ movePointers ++ [Comment "end of newpair"]
transRVal (RPair pe) dst = transPair pe dst <++ [Load (Reg dst) (Ind dst)]
transRVal (Call (AST.Ident i _) es _) dst = do
  movParamInstrs <- concat <$> zipWithM transExp es [IRParam x | x <- [0..]]
  regsInUse <- gets inUse
  return $ movParamInstrs ++ [Jsr i, Mov (Reg dst) (Reg IRRet)] 

-- Puts the address (which we will load into later) into dst
transLVal :: LVal -> IRReg -> IRStatementGenerator IRInstrs
transLVal (LPair pe) dst = withReg (\r -> transPair pe r <++ [Mov (Reg dst) (Reg r)])
transLVal (LArray ae) dst = withReg (\r -> transArrayElem ae r <++ [Mov (Reg dst) (Reg r)])
transLVal _ _ = undefined

-- takes pairElem and puts address of element into dst
transPair :: PairElem -> IRReg -> IRStatementGenerator IRInstrs
transPair (Fst (LIdent (AST.Ident i _)) _) dst' = getVarReg (Ident i) >>= (\r -> checkNull r <++ [Load (Reg dst') (Ind r)])
transPair (Fst (LPair pe') _) dst' = transPair pe' dst' <++> checkNull dst' <++ [Load (Reg dst') (Ind dst')]
transPair (Fst (LArray ae) _) dst' = transArrayElem ae dst' <++> checkNull dst' <++ [Load (Reg dst') (Ind dst')]
transPair (Snd (LIdent (AST.Ident i _)) _) dst' = do
  varReg <- getVarReg (Ident i) 
  aType <- getWType (Ident i)
  checkNull varReg <++ [Load (Reg dst') (ImmOffset varReg (heapTypeSize aType))]
transPair (Snd (LPair pe') _) dst' = transPair pe' dst' <++> checkNull dst' <++ [Load (Reg dst') (ImmOffset dst' (heapTypeSize WUnit))]
transPair (Snd (LArray ae) _) dst' = transArrayElem ae dst' <++> checkNull dst' <++ [Load (Reg dst') (ImmOffset dst' (heapTypeSize WUnit))]

transMallocCall :: Int -> IRReg -> IRStatementGenerator IRInstrs
transMallocCall size dst = return [Mov (Reg (IRParam 0)) (Imm size), Jsr "malloc", Mov (Reg dst) (Reg IRRet)]

transArrayCreation :: Int -> Int -> IRReg -> IRStatementGenerator IRInstrs
transArrayCreation size len dst = do
  sizeReg <- nextFreeReg
  mallocInstrs <- transMallocCall (heapTypeSize WInt + size * len) dst
  makeRegAvailable sizeReg
  return $ mallocInstrs ++ [Mov (Reg sizeReg) (Imm len), Store (Reg sizeReg) (Ind dst), Add (Reg dst) (Reg dst) (Imm $ heapTypeSize WInt)]

checkNull :: IRReg -> IRStatementGenerator IRInstrs
checkNull toCheck = addHelperFunc ErrNull *> (nextLabel <&> (\notNullLabel -> [Cmp (Reg toCheck) (Imm 0), Jge notNullLabel, Jsr (showHelperLabel ErrNull), Define notNullLabel]))

lValWType :: LVal -> IRStatementGenerator WType
lValWType (LIdent (AST.Ident i _)) = getWType (Ident i)
lValWType (LArray (ArrayElem (AST.Ident i _) _ _)) = getWType (Ident i) >>= (\(WArr baseType _) -> return baseType)
lValWType (LPair (Fst lval _)) = lValWType lval >>= (\(WPair wt _) -> return wt)
lValWType (LPair (Snd lval _)) = lValWType lval >>= (\(WPair _ wt) -> return wt)