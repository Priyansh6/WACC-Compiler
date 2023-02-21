{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.ARM.PrettyPrint (showArm) where

import CodeGeneration.ARM.Registers
import CodeGeneration.IR
import qualified Data.Text as T

showArm :: ArmInstrs -> T.Text
showArm intrs = T.unlines (map showInstr intrs)

showInstr :: ArmInstr -> T.Text
showInstr (Define l b) = "" -- If bool is true then this is a .global label
showInstr (StringData l t) = "" -- Creates a .data section with a string constant
showInstr (Load rd a) = ""
showInstr (Store rd a) = ""
showInstr (Mov rd o2) = ""
showInstr (Add rd rn o2) = "ADD " <> T.unwords (map showOp [rd, rn, o2])
showInstr (Sub rd rn o2) = "SUB " <> T.unwords (map showOp [rd, rn, o2])
showInstr (Mul rd rm rs) = "MUL " <> T.unwords (map showOp [rd, rm, rs])
showInstr (Div _ _ _) =
  T.unlines
    [ "bleq _errDivZero",
      "blal __aeabi_idivmod"
    ]
showInstr (Cmp rn o2) = ""
showInstr (Jmp l) = "BAL " <> l
showInstr (Jsr l) = "BLAL " <> l
showInstr (Jl l) = "BLT " <> l
showInstr (Jg l) = "BGT " <> l
showInstr (Jle l) = "BLE " <> l
showInstr (Jge l) = "BGE " <> l
showInstr (Push o) = "PUSH " <> showOp o
showInstr (Pop o) = "POP " <> showOp o
showInstr (Comment t) = "@ " <> t

showOp :: Operand ArmReg -> T.Text
showOp (Reg r) = showArmReg r
showOp (Regs rs) = "{" <> T.intercalate ", " (map showArmReg rs) <> "}"
showOp (Imm i) = "#" <> T.pack (show i)
showOp (Ind r) = "[" <> showArmReg r <> "]"

showArmReg :: ArmReg -> T.Text
showArmReg = T.toLower . T.pack . show
