{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Program (transProg) where

import Debug.Trace

import CodeGeneration.IR 
import CodeGeneration.Statements (transStats)
import CodeGeneration.DataSection
import CodeGeneration.Helpers (generateHelperFuncs, HelperFuncs)
import CodeGeneration.Utils (IRSectionGenerator, Aux(..), wrapScope, numParamRegs, numGeneralRegs)
import Control.Monad.State
import Data.Maybe

import qualified AST
import qualified Data.Map as M
import qualified Data.Set as S

firstParamStackAccess :: Int
firstParamStackAccess = (numGeneralRegs + 2) * 4 -- offset of first stack param due to all general registers and LR and FP being pushed

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
  (bodyInstrs, aux) <- runStateT (transStats ss) (Aux { available = unlimitedRegs, 
                                                        labelId = 0, 
                                                        varLocs = M.empty, 
                                                        sectionName = name, 
                                                        literTable = lt, 
                                                        helperFuncs = S.empty, 
                                                        inUse = [] }) 
  case bodyInstrs of 
    [] -> return (Section dataSection (Body name True [Mov (Reg IRRet) (Imm 0)]), helperFuncs aux)
    _ -> return (Section dataSection (Body name True (bodyInstrs ++ [Mov (Reg IRRet) (Imm 0)])), helperFuncs aux)

transFunc :: AST.Func -> IRSectionGenerator (Section IRReg, HelperFuncs)
transFunc (AST.Func _ (AST.Ident name _) params ss scopeId _) = do
  let (dataSection, lt) = generateDataSection ss name
      numParams = length params
      registerParamMoves = take numParams $ zipWith Mov (map (Reg . TmpReg) [0..numParamRegs-1]) (map (Reg . IRParam) [0..numParamRegs-1])
      stackParamLoads = zipWith Load (map (Reg . TmpReg) [numParamRegs..numParams-1]) (map (ImmOffset IRFP) [firstParamStackAccess, firstParamStackAccess + 4..])
  (bodyInstrs, aux) <- runStateT (transStats ss) (Aux { available = map TmpReg [numParams..],
                                                        labelId = 0, 
                                                        varLocs = M.fromList (zip (map ((\(AST.Ident i _) -> Ident i) . snd) params) (map TmpReg [0..])), 
                                                        sectionName = name, 
                                                        literTable = lt, 
                                                        helperFuncs = S.empty, 
                                                        inUse = map TmpReg [0..numParams-1] })
  return (Section dataSection (Body name False (registerParamMoves ++ stackParamLoads ++ bodyInstrs ++ [Define (name <> "_return")])), helperFuncs aux)

wrapSectionBody :: IRInstrs -> IRInstrs
wrapSectionBody ss = [Push (Regs [IRFP, IRLR]), Mov (Reg IRFP) (Reg IRSP)] ++ ss ++ [Pop (Regs [IRFP, IRPC])]
