module Program (transProg) where

import qualified IR  
import qualified AST

transProg :: AST.Program -> Reader (SymbolTable, ScopeMap) IR.IRInstrs
-- transProg p@(Program fs ss) = transMain ss ++ transFunc fs ++ generateHelperFuncs p
transProg = undefined

-- generate a .data section (potentially) as well as a .text with function body
-- also marches stack pointer
-- calls transStats
transMain :: AST.Stats -> StateT FPOffsets (Reader (SymbolTable, ScopeMap) IR.IRInstrs)
-- transMain m = generalMainSetupStuff ++ generateDataSection m ++ transStats m
transMain = undefined

transFunc :: AST.Func -> IR.IRInstrs
-- transFunc (AST.Func t x params ss) = generalFuncSetupStuff ++ generateDataSection ss ++ transStats ss
transFunc = undefined