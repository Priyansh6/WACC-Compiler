{-# LANGUAGE OverloadedStrings #-}

module AST where

import qualified Data.Text as T 

-- data Program = Program Func Stat
-- data Func = Func Type Ident ParamList Stat

data Expr
  = IntLiter Int
  | BoolLiter Bool
  | CharLiter Char
  | StrLiter T.Text
  | PairLiter 
  | Ident T.Text
  | ArrayElem Ident [Expr]
  | UnaryOp Expr
  | BinOp Expr Expr

data UnaryOp = (:!:) | Neg | Len | Ord | Chr

data BinOp
  = (:*:)
  | (:/:)
  | (:%:)
  | (:+:)
  | (:-:)
  | (:>:)
  | (:>=:)
  | (:<:)
  | (:<=:)
  | (:==:)
  | (:!=:)
  | (:&&:)
  | (:||:)