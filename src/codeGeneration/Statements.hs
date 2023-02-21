module CodeGeneration.Statements (transStats) where

import Control.Monad.Reader
import Control.Monad.State

import CodeGeneration.IR (IRReg, Instrs, FPOffsets)
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)

import qualified AST

-- Needs to carry around symbol table + scope map + offsets of any variable from fp
transStats :: AST.Stats -> StateT FPOffsets (Reader (SymbolTable, ScopeMap)) (Instrs IRReg)
transStats = undefined

-- Pattern match on AST nodes
transStat:: AST.Stat -> StateT FPOffsets (Reader (SymbolTable, ScopeMap)) (Instrs IRReg)
transStat = undefined