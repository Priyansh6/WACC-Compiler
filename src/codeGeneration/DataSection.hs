module CodeGeneration.DataSection (generateDataSection) where

import Control.Monad.Reader (Reader)
import CodeGeneration.IR (IRInstrs)
import Semantic.Type.SymbolTable (SymbolTable)
import Semantic.Rename.Scope (ScopeMap)
import qualified AST

generateDataSection :: AST.Stats -> Reader (SymbolTable, ScopeMap) IRInstrs
generateDataSection = undefined
-- generateDataSection ss = map generateDataSection' ss
--     where
--         generateDataSection' :: AST.Stat -> Reader (SymbolTable, ScopeMap) Instrs IRReg