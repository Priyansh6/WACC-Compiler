module Semantic.Rename.Helpers where

import Data.Set (Set)
import qualified Data.Text as T

import AST

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSndFunc :: b -> (a, b -> c) -> (a, c)
mapSndFunc y (x, f) = (x, f y)

chain :: (acc -> x -> (acc, x)) -> x -> (acc, x -> y) -> (acc, y)
chain accFunc x (acc, f) =
  mapSnd f (accFunc acc x)

addScopeToIdent :: Int -> Ident -> Ident
addScopeToIdent scope (Ident i pos) =
  Ident (T.append (T.snoc i '-') (T.pack (show scope))) pos

getOriginalIdent :: Ident -> Ident
getOriginalIdent (Ident i pos) = Ident (T.takeWhile ('-' /=) i) pos

unpackBinExpr :: Expr -> ((Expr -> Expr -> Position -> Expr), Expr, Expr, Position)
unpackBinExpr ((:*:) e1 e2 pos) = ((:*:), e1, e2, pos)
unpackBinExpr ((:/:) e1 e2 pos) = ((:/:), e1, e2, pos)
unpackBinExpr ((:%:) e1 e2 pos) = ((:%:), e1, e2, pos)
unpackBinExpr ((:+:) e1 e2 pos) = ((:+:), e1, e2, pos)
unpackBinExpr ((:-:) e1 e2 pos) = ((:-:), e1, e2, pos)
unpackBinExpr ((:>:) e1 e2 pos) = ((:>:), e1, e2, pos)
unpackBinExpr ((:>=:) e1 e2 pos) = ((:>=:), e1, e2, pos)
unpackBinExpr ((:<:) e1 e2 pos) = ((:<:), e1, e2, pos)
unpackBinExpr ((:<=:) e1 e2 pos) = ((:<=:), e1, e2, pos)
unpackBinExpr ((:==:) e1 e2 pos) = ((:==:), e1, e2, pos)
unpackBinExpr ((:!=:) e1 e2 pos) = ((:!=:), e1, e2, pos)
unpackBinExpr ((:&&:) e1 e2 pos) = ((:&&:), e1, e2, pos)
unpackBinExpr ((:||:) e1 e2 pos) = ((:||:), e1, e2, pos)
unpackBinExpr a = error $ show a --Should never reach this case

isLiteralExpr :: Expr -> Bool
isLiteralExpr (IntLiter _ _) = True
isLiteralExpr (BoolLiter _ _) = True
isLiteralExpr (CharLiter _ _) = True
isLiteralExpr (StrLiter _ _) = True
isLiteralExpr (PairLiter _ ) = True
isLiteralExpr _ = False