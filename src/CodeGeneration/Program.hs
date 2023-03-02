{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Program (transProg) where

import Debug.Trace

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
  case bodyInstrs of 
    [] -> return (Section dataSection (Body name True [Mov (Reg IRRet) (Imm 0)]), helperFuncs aux)
    _ -> return (Section dataSection (Body name True (bodyInstrs ++ [Mov (Reg IRRet) (Imm 0)])), helperFuncs aux)

transFunc :: AST.Func -> IRSectionGenerator (Section IRReg, HelperFuncs)
transFunc (AST.Func _ (AST.Ident i _) params ss scopeId _) = do
  let unlimitedRegs = map TmpReg [(length params)..]
      name = i
      (dataSection, lt) = generateDataSection ss name
      used = [TmpReg x | x <- [0 .. (length params) - 1]]
      pushParamInstrs = [Mov (Reg (TmpReg x)) (Reg (IRParam x)) | x <- [0 .. (length params) - 1]]
      (_, paramIds) = unzip params
      paramIdents = map (\(AST.Ident id _) -> (Ident id)) paramIds
      paramRegs = zipWith (\id r -> (id, TmpReg r)) paramIdents [0 .. (length params) - 1]
  (bodyInstrs, aux) <- runStateT (transStats ss) (Aux {available = unlimitedRegs, labelId = 0, varLocs = M.fromList paramRegs, sectionName = name, literTable = lt, helperFuncs = S.empty, inUse = used})
  --bodyInstrs' <- wrapScope (fromJust scopeId)  (pushParamInstrs ++ bodyInstrs)
  -- trace (show paramRegs) $ return (Section dataSection (Body name False (pushParamInstrs ++ bodyInstrs)), helperFuncs aux)
  return (Section dataSection (Body name False (pushParamInstrs ++ bodyInstrs ++ [Define (name <> "_return")])), helperFuncs aux)

wrapSectionBody :: IRInstrs -> IRInstrs
wrapSectionBody ss = [Push (Regs [IRFP, IRLR]), Mov (Reg IRFP) (Reg IRSP)] ++ ss ++ [Pop (Regs [IRFP, IRPC])]
