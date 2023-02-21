module Helpers (generateHelperFuncs) where

import qualified AST

-- Generates code for print, println, read etc.
-- should maybe (definitely) mangle names for these so these aren't renamed variables
generateHelperFuncs :: AST.Program -> Reader (SymbolTable, ScopeMap) Instrs IRReg
generateHelpersFuncs = scanHelperFuncs >>= generateHelperFuncs' 
    where
        generateHelperFuncs' :: Set (HelperFunc, Type) -> Reader (SymbolTable, ScopeMap) Instrs IRReg
        generateHelperFuncs' = undefined

scanHelperFuncs :: AST.Program -> Set (HelperFunc, Type)
scanHelperFuncs = undefined