{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration.Statements (transStats) where

import AST hiding (Ident)
import CodeGeneration.IR
import CodeGeneration.Expressions (transExp)
import CodeGeneration.Utils (IRStatementGenerator, nextFreeReg, makeRegAvailable, insertVarReg, getVarReg)
import Control.Monad.State
import Data.Map ((!))

import qualified AST (Ident(Ident)) 

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
transStat (Assign _ _ _) = return []
transStat (Read l _) = return []
transStat (Free e _) = return []
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
transStat (Print e) = return []
transStat (Println e) = return []
transStat (If e ss _ ss' _ _) = return [] 
transStat (While e ss _ _) = return []
transStat (Begin ss _) = transStats ss

transRVal :: RVal -> IRReg -> IRStatementGenerator IRInstrs
transRVal (RExpr e) dst = transExp e dst
transRVal (ArrayLiter es _) dst = return []
transRVal (NewPair e e' _) dst = return []
transRVal (RPair pe) dst = return []
transRVal (Call (AST.Ident i _) es _) dst = return []
 