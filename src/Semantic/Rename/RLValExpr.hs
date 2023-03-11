module Semantic.Rename.RLValExpr (module Semantic.Rename.RLValExpr) where 

import Data.Bool

import AST
import Semantic.Errors
import Semantic.Rename.Utils

renameLVal :: LVal -> Renamer LVal
renameLVal (LIdent i) = LIdent <$> renameDeclaredIdent i
renameLVal (LArray arrayElem) = LArray <$> renameArrayElem arrayElem
renameLVal (LPair pairElem) = LPair <$> renamePairElem pairElem

renameRVal :: RVal -> Renamer RVal
renameRVal (RExpr expr) = RExpr <$> renameExpr expr
renameRVal (ArrayLiter exprs pos) = ArrayLiter <$> mapM renameExpr exprs <*> return pos
renameRVal (NewPair expr1 expr2 pos) = NewPair <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameRVal (RPair pairElem) = RPair <$> renamePairElem pairElem
renameRVal (Call name exprs pos) = do
  identInScope 0 name >>= bool (addSemanticError $ FunctionNotDefined name) (return ())
  Call <$> return name <*> mapM renameExpr exprs <*> return pos

renameExpr :: Expr -> Renamer Expr
renameExpr (IdentExpr i pos) = IdentExpr <$> renameDeclaredIdent i <*> return pos
renameExpr (ArrayExpr arrayElem pos) = ArrayExpr <$> renameArrayElem arrayElem <*> return pos
renameExpr (Not expr pos) = Not <$> renameExpr expr <*> return pos
renameExpr (Neg expr pos) = Neg <$> renameExpr expr <*> return pos
renameExpr (Len expr pos) = Len <$> renameExpr expr <*> return pos
renameExpr (Ord expr pos) = Ord <$> renameExpr expr <*> return pos
renameExpr (Chr expr pos) = Chr <$> renameExpr expr <*> return pos
renameExpr ((:*:) expr1 expr2 pos) = (:*:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:/:) expr1 expr2 pos) = (:/:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:%:) expr1 expr2 pos) = (:%:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:+:) expr1 expr2 pos) = (:+:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:-:) expr1 expr2 pos) = (:-:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:>:) expr1 expr2 pos) = (:>:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:>=:) expr1 expr2 pos) = (:>=:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:<:) expr1 expr2 pos) = (:<:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:<=:) expr1 expr2 pos) = (:<=:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:==:) expr1 expr2 pos) = (:==:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:!=:) expr1 expr2 pos) = (:!=:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:&&:) expr1 expr2 pos) = (:&&:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr ((:||:) expr1 expr2 pos) = (:||:) <$> renameExpr expr1 <*> renameExpr expr2 <*> return pos
renameExpr expr = return expr

renameArrayElem :: ArrayElem -> Renamer ArrayElem
renameArrayElem (ArrayElem i exprs pos) = ArrayElem <$> renameDeclaredIdent i <*> mapM renameExpr exprs <*> return pos

renamePairElem :: PairElem -> Renamer PairElem
renamePairElem (Fst lVal pos) = Fst <$> renameLVal lVal <*> return pos
renamePairElem (Snd lVal pos) = Snd <$> renameLVal lVal <*> return pos

renameDeclaredIdent :: Ident -> Renamer Ident
renameDeclaredIdent name =
  getIdentFromScopeStack name >>= maybe (addSemanticError (VariableNotDefined name) >> return name) return

renameUndeclaredIdent :: Ident -> Renamer Ident
renameUndeclaredIdent name = do
  s <- getCurrentScope
  let name' = addScopeToIdent s name
  identInScope s name' >>= bool 
    (insertIdentInScope s name' >> return name')
    (addSemanticError (VariableAlreadyDefined name) >> return name)