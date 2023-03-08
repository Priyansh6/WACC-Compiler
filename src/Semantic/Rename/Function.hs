module Semantic.Rename.Function (renameFunc) where 

import AST
import Semantic.Rename.Utils
import Semantic.Rename.Statement
import Semantic.Rename.RLValExpr

renameFunc :: Func -> Renamer Func
renameFunc (Func t name params stats _ pos) = do
  params' <- prepareNewScope $ mapM renameParam params
  s <- nextFreeScope
  stats' <- prepareNewScope $ mapM renameStat stats
  return $ Func t name params' stats' (Just s) pos
      
renameParam :: (WType, Ident) -> Renamer (WType, Ident)
renameParam (t, name) = (\n -> (t, n)) <$> renameUndeclaredIdent name