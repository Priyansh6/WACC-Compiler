{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Program (transProg) where

import CodeGeneration.IR 
import CodeGeneration.Statements (transStats)
import CodeGeneration.DataSection
import CodeGeneration.Utils (IRSectionGenerator, Aux(..))
import Control.Monad.State

import qualified AST
import qualified Data.Map as M
import qualified Data.Set as S

transProg :: AST.Program -> IRSectionGenerator (Program IRReg)
transProg (AST.Program fs ss) = do
  mainSection <- transMain ss
  otherSections <- mapM transFunc fs
  return $ otherSections ++ [mainSection]

transMain :: AST.Stats -> IRSectionGenerator (Section IRReg)
transMain ss = do 
  let unlimitedRegs = map TmpReg [0..]
      name = "main"
      (dataSection, lt) = generateDataSection ss name
  bodyInstrs <- evalStateT (transStats ss) (Aux {available = unlimitedRegs, labelId = 0, varLocs = M.empty, sectionName = name, literTable = lt, helperFuncs = S.empty, inUse = []}) 
  return $ Section dataSection (Body name True (wrapSectionBody bodyInstrs))

transFunc :: AST.Func -> IRSectionGenerator (Section IRReg)
transFunc (AST.Func _ (AST.Ident i _) params ss _ _) = do
  let unlimitedRegs = map TmpReg [0..]
      name = i
      (dataSection, lt) = generateDataSection ss name
      pushParamInstrs = [Mov (Reg (TmpReg x)) (Reg (IRParam x)) | x <- [0 .. (length params)]]
  bodyInstrs <- evalStateT (transStats ss) (Aux {available = unlimitedRegs, labelId = 0, varLocs = M.empty, sectionName = name, literTable = lt, helperFuncs = S.empty, inUse = []})
  return $ Section dataSection (Body name True (wrapSectionBody (pushParamInstrs ++ bodyInstrs)))

wrapSectionBody :: IRInstrs -> IRInstrs
wrapSectionBody ss = [Push (Regs [IRFP, IRLR]), Mov (Reg IRFP) (Reg IRSP)] ++ ss ++ [Pop (Regs [IRFP, IRPC])]