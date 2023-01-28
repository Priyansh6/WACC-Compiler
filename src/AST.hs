{-# LANGUAGE OverloadedStrings #-}

module AST where

import qualified Data.Text as T 

-- data Program = Program Func Stat
-- data Func = Func Type Ident ParamList Stat

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

data Ident = Ident T.Text