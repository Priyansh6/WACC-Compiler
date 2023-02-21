module CodeGeneration.Program (transProg) where

import Control.Monad.Reader 
import Control.Monad.State

import CodeGeneration.IR (IRInstrs, FPOffsets)
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)

import qualified AST

transProg :: AST.Program -> Reader (SymbolTable, ScopeMap) IRInstrs
-- transProg p@(Program fs ss) = transMain ss ++ transFunc fs ++ generateHelperFuncs p
transProg = undefined

-- generate a .data section (potentially) as well as a .text with function body
-- also marches stack pointer
-- calls transStats
transMain :: AST.Stats -> StateT FPOffsets (Reader (SymbolTable, ScopeMap)) IRInstrs
-- transMain m = generalMainSetupStuff ++ generateDataSection m ++ transStats m
transMain = undefined

transFunc :: AST.Func -> IRInstrs
-- transFunc (AST.Func t x params ss) = generalFuncSetupStuff ++ generateDataSection ss ++ transStats ss
transFunc = undefined