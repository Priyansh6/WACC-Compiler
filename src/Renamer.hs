module Renamer where

import AST

import Data.Map as M

type VarMap M.Map Ident Ident

renameProg :: Program -> (Program, VarMap)
renameProg (Program funcs stat)
  = Program (renamedFuncs renamedStats)
    where 
      (renamedFuncs, VarMap) = renameFunc M.empty 


renameFuncs :: [Func] -> VarMap -> Int -> ([Func], VarMap, Int)
renameFuncs [] vars i
  = ([], vars, i)
renameFuncs ((Func _ _ params stat ): funcs) vars i
  = 
  where
    renamedParams = foldl renameParam (vars, [], i)

    -- renameParams :: VarMap -> [(WType, Ident)] -> (VarMap, [(WType, Ident)])
    -- renameParams varMap [] = (varMap, [])
    -- renameParams varMap [param : params] 
    --   = (varMap'', params)
    --   where
    --     (varMap', param') = renameParam varMap param
    --     (varMap'', params') = renameParams varMap' params
    
    renameParams :: VarMap -> [(WType, Ident)] -> (VarMap, [(WType, Ident)])
    renameParams = scanl renameParam vars 

    renameParam :: (VarMap, [(WType, Ident)], Int, (WType, Ident)) -> (VarMap, (WType, Ident))
    renameParam varMap (t, name@(Ident id))
      | member name' varMap = error(msg)
      | otherwise           = (varMap', (t, name'))
      where
        msg = "Error: Variable " ++ unpack id ++ " already defined."
        varMap' = insert name' name varMap
        name' = Ident (append id i)