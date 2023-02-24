module CodeGeneration.IR (module CodeGeneration.IR) where

import qualified Data.Map as M
import qualified Data.Text as T

type Program a = [Section a]

data Section a = Section [Data] [Function a]

data Function a = Function Label Global [Instr a]

type Global = Bool

data Data = StringData Label T.Text

data IRReg = TmpReg Int | IRFP | IRSP | IRLR | IRPC | IRRet

type Instrs a = [Instr a]
type IRInstrs = Instrs IRReg

data Ident = Ident T.Text

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
  | Jl Label
  | Jg Label
  | Jle Label
  | Jge Label
  | Push (Operand a)
  | Pop (Operand a)
  | Define Label
  | Comment T.Text -- Creates a comment in the assembly file

type Label = T.Text

data Operand a
  = Reg a
  | Regs [a] -- for Push and Pop
  | Imm Int
  | Abs Label
  | Var Ident
  | Ind a -- register indirect
  | ImmOffset a Int -- for addressing mode 2

type FPOffsets = M.Map Ident Int