module SemanticAnalyser where

import AST

import Data.List(foldl')
import Data.Maybe
import qualified Data.Map as M

data SymbolTable = SymbolTable (Maybe SymbolTable) (M.Map Ident IdentSymbol)

data IdentSymbol = ProgSymbol Program SymbolTable
                 | FuncSymbol

find :: Ident -> SymbolTable -> Maybe IdentSymbol
find id (SymbolTable _ m) = M.lookup id m

findAll :: Ident -> SymbolTable -> Maybe IdentSymbol
findAll id st@(SymbolTable parentSt _)
  | isNothing parentSt = Nothing
  | otherwise          = maybe (findAll id (fromJust parentSt)) Just ids
  where
    ids = find id st

checkProg :: SymbolTable -> Program -> SymbolTable
checkProg st (Program fs ss)
  = checkStat stFuncs ss
    where
      stFuncs = foldl' checkFunc st fs

checkFunc :: SymbolTable -> Func -> SymbolTable
checkFunc = undefined

checkStat :: SymbolTable -> Stats -> SymbolTable
checkStat = undefined

