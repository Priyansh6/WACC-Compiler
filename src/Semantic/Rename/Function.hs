module Semantic.Rename.Function (renameFunc, renameParam) where 

import Control.Monad.Reader

import AST
import Semantic.Rename.Utils
import Semantic.Rename.Statement
import Semantic.Rename.RLValExpr

renameFunc :: Func -> Renamer Func
renameFunc f@(Func t _ params stats _ pos) = do
  s1 <- nextFreeScope
  params' <- prepareNewScope $ mapM renameParam params
  s2 <- nextFreeScope
  stats' <- local (s1:) $ prepareNewScope (mapM renameStat stats)
  return $ Func t (addTypesToFunc f) params' stats' (Just s2) pos
      
renameParam :: (WType, Ident) -> Renamer (WType, Ident)
renameParam (t, name) = (\n -> (t, n)) <$> renameUndeclaredIdent name