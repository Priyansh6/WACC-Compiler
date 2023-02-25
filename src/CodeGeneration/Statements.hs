{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration.Statements (transStats) where

import AST
import CodeGeneration.IR
import CodeGeneration.Expressions (transExp)
import CodeGeneration.Utils (IRStatementGenerator, nextFreeReg, makeRegAvailable)

transStats :: Stats -> IRStatementGenerator IRInstrs
transStats ss = concat <$> mapM transStat ss 

transStat :: Stat -> IRStatementGenerator IRInstrs
transStat Skip = return []
transStat (DecAssign t i r _) = return []
transStat (Assign l r _) = return []
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