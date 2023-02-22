module CodeGeneration.IR 
  ( Instrs, 
    Instr,
    IRReg,
    IRInstr,
    IRInstrs,
    Ident,
    FPOffsets
  )
where

import qualified Data.Map as M
import qualified Data.Text as T

type Program a = [Section a]
data Section a = Section Label [Data] [Instr a]
data Data = StringData Label T.Text

data IRReg = TmpReg Int | IRFP | IRSP | IRLR | IRPC 

type Instrs a = [Instr a]
type IRInstrs = Instrs IRReg

data Ident = Ident T.Text

data Instr a = Load (Operand a) (Operand a)
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

data Operand a = Reg a
               | Regs [a] -- for Push and Pop
               | Imm Int   
               | Abs Label    
               | Ind a  -- register indirect

type FPOffsets = M.Map Ident Int