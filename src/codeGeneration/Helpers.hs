module CodeGeneration.Helpers (generateHelperFuncs) where

import Control.Monad.Reader
import Data.Set (Set)

import CodeGeneration.IR (IRInstrs)
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)
import qualified AST

data HelperFunc = Print | Println | Read 
data FormatType = FInt | FStr 

-- Generates code for print, println, read etc.
-- should mayb (definitely) mangle names for these so these aren't renamed variables
generateHelperFuncs :: AST.Program -> Reader (SymbolTable, ScopeMap) IRInstrs
generateHelperFuncs = undefined
-- generateHelperFuncs = scanHelperFuncs >>= generateHelperFuncs' 
--     where
--         generateHelperFuncs' :: Set (HelperFunc, FormatType) -> Reader (SymbolTable, ScopeMap) IRInstrs
--         generateHelperFuncs' = undefined

scanHelperFuncs :: AST.Program -> Set (HelperFunc, FormatType)
scanHelperFuncs = undefined