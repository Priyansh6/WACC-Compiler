{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Program (transProg) where

import CodeGeneration.IR 
import CodeGeneration.Statements (transStats)
import CodeGeneration.DataSection
import CodeGeneration.Utils (IRSectionGenerator, Aux(Aux, available, labelId, varLocs, sectionName, literTable))
import Control.Monad.State

import qualified AST
import qualified Data.Map as M

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
  bodyInstrs <- evalStateT (transStats ss) (Aux {available = unlimitedRegs, labelId = 0, varLocs = M.empty, sectionName = name, literTable = lt}) 
  case bodyInstrs of
    [] -> return $ Section dataSection (Body name True (wrapSectionBody [Mov (Reg IRRet) (Imm 0)]))
    _ -> return $ Section dataSection (Body name True (wrapSectionBody bodyInstrs))

transFunc :: AST.Func -> IRSectionGenerator (Section IRReg)
transFunc (AST.Func _ (AST.Ident i _) _ _ _ _) = return $ Section [] (Body i False [])

wrapSectionBody :: IRInstrs -> IRInstrs
wrapSectionBody ss = [Push (Regs [IRFP, IRLR]), Mov (Reg IRFP) (Reg IRSP)] ++ ss ++ [Pop (Regs [IRFP, IRPC])]