{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Intermediate.Statements (transStats) where
-- Translate statements from the AST into Intermediate instructions --

import AST hiding (Ident)
import qualified AST (Ident (Ident))
import CodeGeneration.Intermediate.Expressions (transArrayElem, transExp)
import CodeGeneration.Intermediate.Helpers
import CodeGeneration.Intermediate.IR
import CodeGeneration.Utils
import Control.Monad ( zipWithM )
import Control.Monad.State ( gets )
import Data.Functor ((<&>))

mallocLabel :: Label
mallocLabel = "malloc"

pointerSize :: Int
pointerSize = 4

transStats :: Stats -> IRStatementGenerator IRInstrs
transStats ss = concat <$> mapM transStat ss

transStat :: Stat -> IRStatementGenerator IRInstrs
transStat Skip = return []
transStat (DecAssign _ (AST.Ident i _) r _) = do
  varReg <- nextFreeReg
  rInstrs <- transRVal r varReg
  insertVarReg (Ident i) varReg
  return rInstrs
transStat (Assign (LIdent (AST.Ident i _)) r _) = do
  varReg <- getVarReg (Ident i)
  withReg (\rReg -> transRVal r rReg <++ [Mov (Reg varReg) (Reg rReg)])
transStat (Assign l@(LArray (ArrayElem (AST.Ident i _) es@(e : _) _)) r _) = do
  lInstrs <- transLVal l (IRParam 0)
  eInstrs <- transExp e (IRParam 1)
  rInstrs <- transRVal r (IRParam 2)
  wType <- getWType (Ident i)
  let (WArr t dim) = wType
      accType = if dim == length es then t else WArr t (dim - length es)
      tSize = heapTypeSize accType
      hf = if tSize == 1 then ArrStoreB else ArrStore
  return $ lInstrs ++ eInstrs ++ rInstrs ++ [Jsr (showHelperLabel hf)]
transStat (Assign l r _) =
  withReg
    ( \lReg ->
        withReg (\rReg -> transLVal l lReg <++> transRVal r rReg <++ [Store (Reg rReg) (Ind lReg)])
    )
transStat (Read l@(LIdent (AST.Ident i _)) _) = do
  varReg <- getVarReg (Ident i)
  helperFuncType <- lValWType l <&> HRead . fromWType
  addHelperFunc helperFuncType
  return
    [ Mov (Reg (IRParam 0)) (Reg varReg),
      Jsr (showHelperLabel helperFuncType),
      Mov (Reg varReg) (Reg IRRet)
    ]
transStat (Read l _) = do
  helperFuncType <- lValWType l <&> HRead . fromWType
  addHelperFunc helperFuncType
  withReg
    ( \lReg ->
        transLVal l lReg
          <++ [ Mov (Reg (IRParam 0)) (Reg lReg),
                Jsr (showHelperLabel helperFuncType),
                Mov (Reg lReg) (Reg IRRet)
              ]
    )
transStat (Free e _) = do
  refReg <- nextFreeReg
  evalRefInstrs <- transExp e refReg
  eType <- exprType e
  makeRegAvailable refReg
  case eType of
    WPair _ _ ->
      addHelperFunc FreePair
        >> evalRefInstrs ++> checkNull refReg
          <++ [ Mov (Reg IRRet) (Reg refReg),
                Jsr (showHelperLabel FreePair)
              ]
    WArr _ _ ->
      addHelperFunc FreeArr
        >> return
          ( evalRefInstrs
              ++ [ Mov (Reg IRRet) (Reg refReg),
                   Jsr (showHelperLabel FreeArr)
                 ]
          )
    _ -> undefined
transStat (Return e _) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  returnFuncName <- getFuncReturnFuncName
  makeRegAvailable dst
  return $ eis ++ [Mov (Reg IRRet) (Reg dst), Jmp returnFuncName]
transStat (Exit e _) = do
  dst <- nextFreeReg
  eis <- transExp e dst
  makeRegAvailable dst
  return $ eis ++ [Mov (Reg $ IRParam 0) (Reg dst), Jsr "exit"]
transStat (Print e) = do
  helperFuncType <- HPrint . fromWType <$> exprType e
  addHelperFunc helperFuncType
  transExp e (IRParam 0) <++ [Jsr (showHelperLabel helperFuncType)]
transStat (Println e) = do
  helperFuncType <- HPrint . fromWType <$> exprType e
  addHelperFunc helperFuncType
  addHelperFunc HPrintln
  transExp e (IRParam 0) <++ [Jsr (showHelperLabel helperFuncType), Jsr (showHelperLabel HPrintln)]
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

-- transRVal translates rval and puts the VALUE into dst
transRVal :: RVal -> IRReg -> IRStatementGenerator IRInstrs
transRVal (RExpr e) dst = transExp e dst
transRVal (ArrayLiter [] _) dst = transArrayCreation 0 0 dst
transRVal (ArrayLiter elems@(e : _) _) dst = do
  eType <- exprType e
  let eSize = heapTypeSize eType
      helperFunc = if eSize == 1 then ArrStoreB else ArrStore
  addHelperFunc helperFunc
  transArrayCreation eSize (length elems) dst <++> (concat <$> zipWithM (transArrLiterElem helperFunc) elems [0 ..])
  where
    transArrLiterElem :: HelperFunc -> Expr -> Int -> IRStatementGenerator IRInstrs
    transArrLiterElem hf e' idx = do
      eInstrs <- transExp e' (IRParam 2)
      return $ [Mov (Reg (IRParam 0)) (Reg dst), Mov (Reg (IRParam 1)) (Imm idx)] ++ eInstrs ++ [Jsr (showHelperLabel hf)]
transRVal (NewPair e e' _) dst = do
  eReg <- nextFreeReg
  ePtrReg <- nextFreeReg
  eReg' <- nextFreeReg
  ePtrReg' <- nextFreeReg
  eType <- exprType e
  eType' <- exprType e'
  let pType = AST.WPair eType eType'
      movePointers = [Store (Reg ePtrReg) (ImmOffset dst 0), Store (Reg ePtrReg') (ImmOffset dst pointerSize)]
  mallocFst <- transMallocCall (heapTypeSize eType) ePtrReg
  mallocSnd <- transMallocCall (heapTypeSize eType') ePtrReg'
  mallocPair <- transMallocCall (heapTypeSize pType) dst
  evalFstInstrs <- transExp e eReg <++ mallocFst ++ [Store (Reg eReg) (Ind ePtrReg)] -- not sure if Ind as the dst of a Mov is dodgy or not
  evalSndInstrs <- transExp e' eReg' <++ mallocSnd ++ [Store (Reg eReg') (Ind ePtrReg')]
  makeRegsAvailable [eReg, eReg', ePtrReg, ePtrReg']
  return $ evalFstInstrs ++ evalSndInstrs ++ mallocPair ++ movePointers ++ [Comment "end of newpair"]
transRVal (RPair pe) dst = transPair pe dst <++ [Load (Reg dst) (Ind dst)]
transRVal (Call (AST.Ident i _) exps _) dst = do
  usedRs <- gets inUse
  let paramsInUse = [irp | irp@(IRParam _) <- usedRs]
      pushParams = [Push (Regs paramsInUse) | not (null paramsInUse)]
      popParams = [Pop (Regs paramsInUse) | not (null paramsInUse)]
  (preparedParams, sOff) <- prepareParams (reverse exps) (numParams - 1) 0
  addHelperFunc ErrOverflow
  return $ pushParams ++ preparedParams ++ [Jsr i, Mov (Reg dst) (Reg IRRet), Add (Reg IRSP) (Reg IRSP) (Imm sOff)] ++ popParams
  where
    numParams = length exps

    prepareParams :: [Expr] -> Int -> Int -> IRStatementGenerator (IRInstrs, Int)
    prepareParams [] _ stackOff = return ([], stackOff)
    prepareParams (e : es) paramNum stackOff = do
      paramInstrs <- transExp e IRScratch1
      (remainingInstrs, finalStackOff) <- prepareParams es (paramNum - 1) stackOff'
      return (paramInstrs ++ [Push (Regs [IRScratch1])] ++ remainingInstrs ++ popInstrs, finalStackOff)
      where
        popInstrs = [Pop (Regs [IRParam paramNum]) | paramNum < numParamRegs]
        stackOff' = if paramNum >= numParamRegs then stackOff + 4 else stackOff

-- Puts the address (which we will load into later) into dst
transLVal :: LVal -> IRReg -> IRStatementGenerator IRInstrs
transLVal (LPair pe) dst = transPair pe dst
transLVal (LArray ae) dst = transArrayElem ae dst
transLVal _ _ = undefined

-- takes pairElem and puts address of element into dst
transPair :: PairElem -> IRReg -> IRStatementGenerator IRInstrs
transPair (Fst (LIdent (AST.Ident i _)) _) dst' =
  getVarReg (Ident i)
    >>= (\r -> checkNull r <++ [Load (Reg dst') (Ind r)])
transPair (Snd (LIdent (AST.Ident i _)) _) dst' =
  getVarReg (Ident i)
    >>= ( \r ->
            checkNull r
              <++ [ Load (Reg dst') (ImmOffset r pointerSize)
                  ]
        )
transPair (Fst (LPair pe') _) dst' = transPair pe' dst' <++> checkNull dst' <++ [Load (Reg dst') (Ind dst')]
transPair (Snd (LPair pe') _) dst' = transPair pe' dst' <++> checkNull dst' <++ [Load (Reg dst') (ImmOffset dst' pointerSize)]
transPair (Fst (LArray ae) _) dst' = transArrayElem ae dst' <++> checkNull dst' <++ [Load (Reg dst') (Ind dst')]
transPair (Snd (LArray ae) _) dst' = transArrayElem ae dst' <++> checkNull dst' <++ [Load (Reg dst') (ImmOffset dst' pointerSize)]

transMallocCall :: Int -> IRReg -> IRStatementGenerator IRInstrs
transMallocCall size dst = return [Mov (Reg (IRParam 0)) (Imm size), Jsr mallocLabel, Mov (Reg dst) (Reg IRRet)]

transArrayCreation :: Int -> Int -> IRReg -> IRStatementGenerator IRInstrs
transArrayCreation size len dst = do
  sizeReg <- nextFreeReg
  mallocInstrs <- transMallocCall (heapTypeSize WInt + size * len) dst
  makeRegAvailable sizeReg
  addHelperFunc ErrOverflow
  return $
    mallocInstrs
      ++ [ Mov (Reg sizeReg) (Imm len),
           Store (Reg sizeReg) (Ind dst),
           Add (Reg dst) (Reg dst) (Imm $ heapTypeSize WInt)
         ]

checkNull :: IRReg -> IRStatementGenerator IRInstrs
checkNull toCheck =
  addHelperFunc ErrNull
    *> ( nextLabel
           <&> ( \notNullLabel ->
                   [ Cmp (Reg toCheck) (Imm 0),
                     Jge notNullLabel,
                     Jsr (showHelperLabel ErrNull),
                     Define notNullLabel
                   ]
               )
       )

lValWType :: LVal -> IRStatementGenerator WType
lValWType (LIdent (AST.Ident i _)) = getWType (Ident i)
lValWType (LArray (ArrayElem (AST.Ident i _) _ _)) = getWType (Ident i) >>= (\(WArr baseType _) -> return baseType)
lValWType (LPair (Fst lval _)) = lValWType lval >>= (\(WPair wt _) -> return wt)
lValWType (LPair (Snd lval _)) = lValWType lval >>= (\(WPair _ wt) -> return wt)