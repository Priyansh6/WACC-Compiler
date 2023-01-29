{-# LANGUAGE OverloadedStrings #-}

module AST where

import qualified Data.Text as T 

-- data Program = Program Func Stat
-- data Func = Func Type Ident ParamList Stat

data Stat
  = Skip
  | DecAssign WType Ident RVal
  | Assign LVal RVal
  | Read LVal
  | Free Expr
  | Return Expr
  | Exit Expr
  | Print Expr
  | Println Expr
  | If Expr Stat Stat
  | While Expr Stat
  | Begin Stat
  | Seq Stat Stat

-- An AST node representing all possible WACC types
-- N.B. WUnit is used to allow type erasure within pairs
data WType
  = WUnit 
  | WInt
  | WBool
  | WChar
  | WStr
  | WArr WType
  | WPair WType WType

data LVal
  = LIdent Ident
  | LArray ArrayElem
  | LPair PairElem

data RVal
  = RExpr Expr
  | ArrayLiter [Expr]
  | NewPair Expr Expr
  | RPair PairElem
  | Call Ident [Expr]

data Expr
  = IntLiter Int
  | BoolLiter Bool
  | CharLiter Char
  | StrLiter T.Text
  | PairLiter 
  | IdentExpr Ident 
  | ArrayExpr ArrayElem
  | UnaryOp Expr
  | BinOp Expr Expr

data Ident = Ident T.Text
data ArrayElem = ArrayElem Ident [Expr]
data PairElem
  = Fst LVal
  | Snd LVal

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