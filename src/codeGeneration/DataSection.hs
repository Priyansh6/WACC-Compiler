module DataSection (generateDataSection) where

generateDataSection :: AST.Stats -> Reader (SymbolTable, ScopeMap) Instrs IRReg
generateDataSection = undefined
-- generateDataSection ss = map generateDataSection' ss
--     where
--         generateDataSection' :: AST.Stat -> Reader (SymbolTable, ScopeMap) Instrs IRReg