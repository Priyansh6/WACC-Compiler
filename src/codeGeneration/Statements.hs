module Statements (transStats) where

-- Needs to carry around symbol table + scope map + offsets of any variable from fp
transStats :: AST.Stats -> StateT FPOffsets (Reader (SymbolTable, ScopeMap) (Instrs IRReg))

-- Pattern match on AST nodes
transStat :: AST.Stat -> StateT FPOffsets (Reader (SymbolTable, ScopeMap) (Instrs IRReg))