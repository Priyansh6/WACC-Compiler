{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Program (transProg) where

import CodeGeneration.IR 
import CodeGeneration.Statements (transStats)
import CodeGeneration.DataSection
import CodeGeneration.Helpers (generateHelperFuncs, HelperFuncs)
import CodeGeneration.Utils (IRSectionGenerator, Aux(..), wrapScope)
import Control.Monad.State
import Data.Maybe

import qualified AST
import qualified Data.Map as M
import qualified Data.Set as S

transProg :: AST.Program -> IRSectionGenerator (Program IRReg)
transProg (AST.Program fs ss) = do
  (mainSection, hfs) <- transMain ss
  (otherSections, hfs') <- unzip <$> mapM transFunc fs
  return $ otherSections ++ [mainSection] ++ generateHelperFuncs (S.union hfs (S.unions hfs'))

transMain :: AST.Stats -> IRSectionGenerator (Section IRReg, HelperFuncs)
transMain ss = do 
  let unlimitedRegs = map TmpReg [0..]
      name = "main"
      (dataSection, lt) = generateDataSection ss name
  (bodyInstrs, aux) <- runStateT (transStats ss) (Aux {available = unlimitedRegs, labelId = 0, varLocs = M.empty, sectionName = name, literTable = lt, helperFuncs = S.empty, inUse = []}) 
  bodyInstrs' <- wrapScope 0 bodyInstrs
  case bodyInstrs of 
    [] -> return (Section dataSection (Body name True (wrapSectionBody [Mov (Reg IRRet) (Imm 0)])), helperFuncs aux)
    _ -> return (Section dataSection (Body name True (wrapSectionBody bodyInstrs')), helperFuncs aux)

transFunc :: AST.Func -> IRSectionGenerator (Section IRReg, HelperFuncs)
transFunc (AST.Func _ (AST.Ident i _) params ss scopeId _) = do
  let unlimitedRegs = map TmpReg [0..]
      name = i
      (dataSection, lt) = generateDataSection ss name
      pushParamInstrs = [Mov (Reg (TmpReg x)) (Reg (IRParam x)) | x <- [0 .. (length params)]]
  (bodyInstrs, aux) <- runStateT (transStats ss) (Aux {available = unlimitedRegs, labelId = 0, varLocs = M.empty, sectionName = name, literTable = lt, helperFuncs = S.empty, inUse = []})
  bodyInstrs' <- wrapScope (fromJust scopeId)  (pushParamInstrs ++ bodyInstrs)
  return (Section dataSection (Body name True (wrapSectionBody bodyInstrs')), helperFuncs aux)

wrapSectionBody :: IRInstrs -> IRInstrs
wrapSectionBody ss = [Push (Regs [IRFP, IRLR]), Mov (Reg IRFP) (Reg IRSP)] ++ ss ++ [Pop (Regs [IRFP, IRPC])]
