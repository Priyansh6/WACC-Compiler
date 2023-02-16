module AST (module AST) where

import qualified Data.Text as T 

type Position = (Int, Int)

type Scope = Int

data Program = Program [Func] Stats deriving (Show, Eq)

data Func = Func WType Ident [(WType, Ident)] Stats Scope Position deriving (Show, Eq)

type Stats = [Stat]

data Stat
  = Skip
  | DecAssign WType Ident RVal Position
  | Assign LVal RVal Position
  | Read LVal Position
  | Free Expr Position
  | Return Expr Position
  | Exit Expr Position
  | Print Expr
  | Println Expr
  | If Expr Stats Scope Stats Scope Position 
  | While Expr Stats Scope Position
  | Begin Stats Scope
  deriving (Show, Eq)

-- An AST node representing all possible WACC types
-- N.B. WUnit is used to allow type erasure within pairs
data WType
  = WUnit 
  | WInt
  | WBool
  | WChar
  | WStr
  | WArr WType Int
  | WPair WType WType
  deriving (Show, Eq)

data LVal
  = LIdent Ident
  | LArray ArrayElem
  | LPair PairElem
  deriving (Show, Eq)

data RVal
  = RExpr Expr
  | ArrayLiter [Expr] Position
  | NewPair Expr Expr Position
  | RPair PairElem
  | Call Ident [Expr] Position
  deriving (Show, Eq)

data Expr
  = IntLiter Integer Position
  | BoolLiter Bool Position
  | CharLiter Char Position
  | StrLiter T.Text Position
  | PairLiter Position
  | IdentExpr Ident Position
  | ArrayExpr ArrayElem Position
  | Not Expr Position
  | Neg Expr Position
  | Len Expr Position
  | Ord Expr Position
  | Chr Expr Position
  | (:*:) Expr Expr Position
  | (:/:) Expr Expr Position
  | (:%:) Expr Expr Position
  | (:+:) Expr Expr Position
  | (:-:) Expr Expr Position
  | (:>:) Expr Expr Position
  | (:>=:) Expr Expr Position
  | (:<:) Expr Expr Position
  | (:<=:) Expr Expr Position
  | (:==:) Expr Expr Position
  | (:!=:) Expr Expr Position
  | (:&&:) Expr Expr Position
  | (:||:) Expr Expr Position
  deriving (Show, Eq)

data Ident = Ident T.Text Position deriving (Show)

instance Ord Ident where
  compare (Ident t _) (Ident t' _) = compare t t'

instance Eq Ident where
  (Ident t _) == (Ident t' _) = t == t'

data ArrayElem = ArrayElem Ident [Expr] Position deriving (Show, Eq)

data PairElem
  = Fst LVal Position
  | Snd LVal Position
  deriving (Show, Eq)