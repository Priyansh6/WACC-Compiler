{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.ARM.PrettyPrint (showArm) where

import CodeGeneration.ARM.Registers
import CodeGeneration.IR
import qualified Data.Text as T

showArm :: ArmInstrs -> T.Text
showArm intrs =
  T.unlines $
    [ ".data",
      ".text",
      ".global main",
      "main:"
    ]
      ++ map ((`T.snoc` '\t') . T.unwords . showInstr) intrs

showInstr :: ArmInstr -> [T.Text]
showInstr (Define l b) = [] -- If bool is true then this is a .global label
showInstr (StringData l t) = [] -- Creates a .data section with a string constant
showInstr (Load rd a) = "LDR" : showOps [rd, a]
showInstr (Store rd a) = "STR" : showOps [rd, a]
showInstr (Mov rd o2) = "MOV" : showOps [rd, o2]
showInstr (Add rd rn o2) = "ADD" : showOps [rd, rn, o2]
showInstr (Sub rd rn o2) = "SUB" : showOps [rd, rn, o2]
showInstr (Mul rd rm rs) = "MUL" : showOps [rd, rm, rs]
showInstr (Div _ _ _) = []
showInstr (Cmp rn o2) = "CMP" : showOps [rn, o2]
showInstr (Jmp l) = ["B", l]
showInstr (Jsr l) = ["BL", l]
showInstr (Jl l) = ["BLT", l]
showInstr (Jg l) = ["BGT", l]
showInstr (Jle l) = ["BLE", l]
showInstr (Jge l) = ["BGE", l]
showInstr (Push o) = ["PUSH", showOp o]
showInstr (Pop o) = ["POP", showOp o]
showInstr (Comment t) = ["@", t]

showOp :: Operand ArmReg -> T.Text
showOp (Reg r) = showArmReg r
showOp (Regs rs) = "{" <> T.intercalate ", " (map showArmReg rs) <> "}"
showOp (Imm i) = "#" <> T.pack (show i)
showOp (Ind r) = "[" <> showArmReg r <> "]"

showOps :: [Operand ArmReg] -> [T.Text]
showOps = map showOp

showArmReg :: ArmReg -> T.Text
showArmReg = T.toLower . T.pack . show
