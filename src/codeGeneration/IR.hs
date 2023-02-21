module IR 
  ( Instrs, 
    Instr 
  )
where

import qualified Data.Text as T

data IRReg = TmpReg Int | IRFP | IRSP | IRLR | IRPC 
data ArmReg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC

type ArmInstr = Instr ArmReg
type IRInstr = Instr IRReg
type Instrs a = [Instr a]

data Instr a = Define Label Bool   -- If bool is true then this is a .global label
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
             | Jlt Label
             | Jgt Label
             | Jle Label
             | Jge Label
             | Push (Operand a)
             | Pop (Operand a)
             | Comment T.Text -- Creates a comment in the assembly file

type Label = T.Text

data Operand a = Reg a
               | Imm Int   
               | Abs Label    
               | Ind IRReg  -- register indirect