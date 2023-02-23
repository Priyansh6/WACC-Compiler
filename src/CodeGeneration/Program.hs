{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Program (transProg) where

import Control.Monad.Reader 
import Control.Monad.State

import CodeGeneration.IR 
import CodeGeneration.Statements (transStats, Aux(Aux, available))
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)

import qualified AST

transProg :: AST.Program -> Reader (SymbolTable, ScopeMap) (Program IRReg)
transProg (AST.Program fs ss) = do
  mainSection <- transMain ss
  otherSections <- mapM transFunc fs
  return $ otherSections ++ [mainSection]

transMain :: AST.Stats -> (Reader (SymbolTable, ScopeMap)) (Section IRReg)
transMain ss = do 
  let unlimitedRegs = map TmpReg [0..]
  bodyInstrs <- evalStateT (transStats ss) (Aux {available = unlimitedRegs}) 
  return $ Section [] [Function "main" True (wrapSectionBody bodyInstrs)]

transFunc :: AST.Func -> (Reader (SymbolTable, ScopeMap)) (Section IRReg)
transFunc _ = return $ Section [] []

wrapSectionBody :: IRInstrs -> IRInstrs
wrapSectionBody ss = [Push (Regs [IRFP, IRLR]), Mov (Reg IRFP) (Reg IRSP)] ++ ss ++ [Pop (Regs [IRFP, IRPC])]