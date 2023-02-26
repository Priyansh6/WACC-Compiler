module CodeGeneration.IR (module CodeGeneration.IR) where

import qualified Data.Map as M
import qualified Data.Text as T

type Program a = [Section a]

-- data Section a = Section [Data] [Function a] deriving (Show, Eq)
data Section a = Section [Data] (Body a) deriving (Show, Eq)

data Body a = Body Label Global [Instr a] deriving (Show, Eq)

type Global = Bool

data Data = StringData Label T.Text deriving (Show, Eq)

data IRReg = TmpReg Int | IRFP | IRSP | IRLR | IRPC | IRRet deriving (Show, Eq)

type Instrs a = [Instr a]
type IRInstrs = Instrs IRReg

data Ident = Ident T.Text deriving (Show, Ord, Eq)

data Instr a
  = Load (Operand a) (Operand a)
  | Store (Operand a) (Operand a)
  | Mov (Operand a) (Operand a)
  | Add (Operand a) (Operand a) (Operand a)
  | Sub (Operand a) (Operand a) (Operand a)
  | Mul (Operand a) (Operand a) (Operand a)
  | Div (Operand a) (Operand a) (Operand a)
  | Cmp (Operand a) (Operand a)
  | Jmp Label -- Jump to generic label
  | Jsr Label -- Jump to subroutine (updates LR)
  | Je Label
  | Jne Label
  | Jl Label
  | Jg Label
  | Jle Label
  | Jge Label
  | Push (Operand a)
  | Pop (Operand a)
  | Define Label
  | Comment T.Text -- Creates a comment in the assembly file
  deriving (Show, Eq)

type Label = T.Text

data Operand a
  = Reg a
  | Regs [a] -- for Push and Pop
  | Imm Int
  | Abs Label
  | Var Ident
  | Ind a -- register indirect
  | ImmOffset a Int -- for addressing mode 2
  deriving (Show, Eq)

type FPOffsets = M.Map Ident Int