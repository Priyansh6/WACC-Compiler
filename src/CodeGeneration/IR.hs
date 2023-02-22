module CodeGeneration.IR (module CodeGeneration.IR) where

import qualified Data.Map as M
import qualified Data.Text as T

data IRReg = TmpReg Int | FP | SP | LR | PC

type Instrs a = [Instr a]

type IRInstr = Instr IRReg

type IRInstrs = Instrs IRInstr

data Ident = Ident T.Text

data Instr a
  = Define Label Bool -- If bool is true then this is a .global label
  | StringData Label T.Text -- Creates a .data section with a string constant
  | Load (Operand a) (Operand a)
  | Store (Operand a) (Operand a)
  | Mov (Operand a) (Operand a)
  | Add (Operand a) (Operand a) (Operand a)
  | Sub (Operand a) (Operand a) (Operand a)
  | Mul (Operand a) (Operand a) (Operand a)
  | Div (Operand a) (Operand a) (Operand a)
  | Cmp (Operand a) (Operand a)
  | Jmp Label -- Jump to generic label
  | Jsr Label -- Jump to subroutine (updates LR)
  | Jl Label
  | Jg Label
  | Jle Label
  | Jge Label
  | Push (Operand a)
  | Pop (Operand a)
  | Comment T.Text -- Creates a comment in the assembly file

type Label = T.Text

data Operand a
  = Reg a
  | Regs [a] -- for Push and Pop
  | Imm Int
  | Abs Label
  | Ind a -- register indirect

type FPOffsets = M.Map Ident Int