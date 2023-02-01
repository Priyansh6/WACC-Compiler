module AST where

import qualified Data.Text as T 

data Program = Program [Func] Stat deriving (Show, Eq)
data Func = Func WType Ident [(WType, Ident)] Stat deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data LVal
  = LIdent Ident
  | LArray ArrayElem
  | LPair PairElem
  deriving (Show, Eq)

data RVal
  = RExpr Expr
  | ArrayLiter [Expr]
  | NewPair Expr Expr
  | RPair PairElem
  | Call Ident [Expr]
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data Ident = Ident T.Text deriving (Show, Eq, Ord)
data ArrayElem = ArrayElem Ident [Expr] deriving (Show, Eq)
data PairElem
  = Fst LVal
  | Snd LVal
  deriving (Show, Eq)

data UnaryOp = (:!:) | Neg | Len | Ord | Chr
               deriving (Show, Eq)

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
  deriving (Show, Eq)