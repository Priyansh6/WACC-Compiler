module IR 
  ( Instrs, 
    Instr 
  )
where

import qualified Data.Text as T

type Instrs = [Instr]

newtype IRReg = TmpReg Int 

data Instr = Define Label Bool   -- if bool is true then this is a .global label
           | StringData Label T.Text -- creates a .data section with a string constant
           | Load Operand Operand 
           | Store Operand Operand 
           | Mov Operand Operand
           | Add Operand Operand Operand
           | Sub Operand Operand Operand
           | Mul Operand Operand Operand
           | Div Operand Operand Operand
           | Cmp Operand Operand 
           | Jmp Label -- Jump to generic label
           | Jsr Label -- Jump to subroutine (updates LR)
           | Jlt Label
           | Jgt Label
           | Jle Label
           | Jge Label
           | Push Operand
           | Pop Operand
           | Comment T.Text

type Label = T.Text

data Operand = Reg IRReg
             | Imm Int   
             | Abs Label    
             | Ind IRReg  -- register indirect