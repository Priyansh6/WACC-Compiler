{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

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

newtype Ident = Text