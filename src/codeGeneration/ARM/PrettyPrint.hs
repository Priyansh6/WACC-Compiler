{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration.ARM.PrettyPrint (showArm) where

import qualified Data.Text as T

import CodeGeneration.ARM.Registers
import CodeGeneration.IR

showArm :: ArmInstrs -> T.Text
showArm intrs = T.intercalate "\n" (map showInstr intrs)

showInstr :: ArmInstr -> T.Text
showInstr (Define l b) = "" -- If bool is true then this is a .global label
showInstr (StringData l t) = "" -- Creates a .data section with a string constant
showInstr (Load rd a) = ""
showInstr (Store rd a) = ""
showInstr (Mov rd o2) = ""
showInstr (Add rd rn o2) = ""
showInstr (Sub rd rn o2) = ""
showInstr (Mul rd rm rs) = ""
showInstr (Div _ _ _) = "" -- ?
showInstr (Cmp rn o2) = ""
showInstr (Jmp l) = "BAL " <> l -- Jump to generic label
showInstr (Jsr l) = "BLAL " <> l -- Jump to subroutine (updates LR)
showInstr (Jl l) = "BLT " <> l
showInstr (Jg l) = "BGT " <> l
showInstr (Jle l) = "BLE " <> l
showInstr (Jge l) = "BGE " <> l
showInstr (Push o) = ""
showInstr (Pop o) = ""
showInstr (Comment t) = "@ " <> t
